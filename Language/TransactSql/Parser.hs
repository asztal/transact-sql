module Language.TransactSql.Parser
       ( select
       , run
       ) where

import Control.Applicative ((<$>), (<|>), (<*), (<*>), (*>), (<$), many, pure)
import Control.Monad (when) 

import Text.Parsec hiding ((<|>), many)
import Text.Parsec.Combinator
import Text.Parsec.Expr
import Text.Parsec.Perm (permute, (<$?>), (<|?>))
import qualified Text.Parsec.Char as C
import qualified Text.Parsec.Token as T

import Language.TransactSql.Types
import Language.TransactSql.AST
import Language.TransactSql.Loc

import qualified System.Directory as D
import Control.Monad (forM)
import System.FilePath ((</>))
import Data.Char (toLower, isSpace)
import Control.Monad (mzero)
import Data.Function (on)
import Data.List.Split (splitWhen)

type P a = Parsec String () a

sqlDef :: T.LanguageDef ()
sqlDef = T.LanguageDef
         { T.commentStart = "/*"
         , T.commentEnd = "*/"
         , T.commentLine = "--"
         , T.nestedComments = False
         , T.identStart = letter <|> char '_'
         , T.identLetter = alphaNum <|> char '_'
         , T.opStart = C.oneOf "-=+*&^%/<>~|."
         , T.opLetter = C.oneOf "-=+*&^%/<>~|."
         , T.reservedNames = ["begin", "by", "case", "cast", "declare", "end", "from", "group", "having", "into", "is", "null", "over", "partition", "role", "schema", "table", "view"]
         , T.reservedOpNames = []
         , T.caseSensitive = False
         }

lexer = T.makeTokenParser sqlDef
identifier = T.identifier lexer
symbol = T.symbol lexer
reserved = T.reserved lexer
operator = T.operator lexer
reservedOp = T.reservedOp lexer
lexeme = T.lexeme lexer
whiteSpace = T.whiteSpace lexer
parens = T.parens lexer
squares = T.brackets lexer

boundedInt :: Integer -> Integer -> P Int
boundedInt low high = do
  n <- T.integer lexer
  when (n > high || n < low) $
    fail ("expected number between " ++ show low ++ " and " ++ show high)
  return (fromIntegral n)

var = char '@' *> identifier
serverVar = char '@' *> char '@' *> identifier

comma = symbol ","
period = symbol "."
star = symbol "*"
equals = symbol "="
semicolon = symbol ";"

loc :: P a -> P (Located a)
loc p = do
  start <- getPosition
  value <- p
  end <- getPosition
  return $ L (mkSpan start end) value

keyword :: String -> P Keyword
keyword word = Keyword <$> loc (reserved word >> pure word)

keywords :: [String] -> P Keyword
keywords words = Keyword <$> loc (mapM_ reserved words >> pure (unwords words))
                 <?> show (unwords words)

literal :: P Literal
literal = (IntL <$> loc (T.integer lexer) <?> "integer literal")
          <|> (StrL <$> loc (quoted '\'' '\'') <?> "string literal")
          <|> (NullL <$> keyword "null" <?> "null")

typeName :: P SqlType
typeName = (identifier >>= rest) <?> "type name"
  where rest "int" = pure Int
        rest "char" = Char <$> len
        rest "nchar" = NChar <$> len
        rest "varchar" = VarChar <$> len
        rest "nvarchar" = NVarChar <$> len
        rest "binary" = Binary <$> len
        rest "varbinary" = VarBinary <$> len
        rest name = fail $ "unknown type name: " ++ name

        len = parens (reserved "max" *> pure (-1)
                      <|> boundedInt 1 0x7FFFFFFF)

term :: P Expr
term = unLoc <$> parens expr
       <|> (varOrServerVar <?> "variable name")
       <|> ELit <$> loc literal
       <|> reserved "cast" *> parens (ECast <$> expr <*> (reserved "as" *> loc typeName))
       <|> caseExpr
       <|> try (EColRef . Just <$> (objectName <* period) <*> ident)
       <|> EColRef Nothing <$> ident
  where caseExpr = reserved "case" *> (condCase <|> exprCase) <* reserved "end"
        condCase = ECondCase <$> many1 (branch (loc cond)) <*> final
        exprCase = EExprCase <$> expr <*> many1 (branch expr) <*> final
        branch on = (,) <$> (reserved "when" *> on) <*> (reserved "then" *> expr)
        final = optionMaybe (reserved "else" *> expr)
        varOrServerVar = char '@' *> (EVar <$> ident
                                      <|> EServerVar <$> (char '@' *> ident))

expr :: P (Located Expr)
expr = loc (unLoc <$> buildExpressionParser ops (loc term)) <?> "expression"
  where ops = [ [ prefix "~" UInvert ]
              , [ binary "*" BMul AssocLeft
                , binary "/" BDiv AssocLeft
                , binary "%" BMod AssocLeft ]
              , [ prefix "+" UPlus
                , prefix "-" UNegate
                , binary "+" BAdd AssocLeft
                , binary "-" BSub AssocLeft
                , binary "%" BAnd AssocLeft
                , binary "^" BXor AssocLeft
                , binary "|" BOr AssocLeft
                ]
              ]

        --binary :: String -> (Located Expr -> Located Expr -> Expr) -> Assoc -> Operator String () Identity (Located Expr)
        -- TODO: Can probably just use noSpan here... Let the root of expr do a `loc`
        -- Much cleaner than this mess.
        binary name binOp assoc
          = Infix (do L lb op <- loc (reservedOp name)
                      let f xx@(L l x) yy@(L l' y)
                            = L (combineSpan l l') (EBin (L lb binOp) xx yy)
                      return f) assoc
        prefix name unOp
          = Prefix $ do L lu op <- loc (reservedOp name)
                        return (\xx@(L l x) -> L (combineSpan lu l) (EUnary (L lu unOp) xx))

cond :: P Cond
cond = CPred <$> predicate
       <|> CNot <$> keyword "not" <*> loc cond
       <?> "condition"

comparison :: P Comparison
comparison = reservedOp "=" *> pure CEq
         <|> reservedOp ">" *> pure CGt
         <|> reservedOp "<" *> pure CLt
         <|> reservedOp ">=" *> pure CGte
         <|> reservedOp "<=" *> pure CLte
         <|> reservedOp "<>" *> pure CNeq

predicate :: P Pred
predicate = flip PCompare <$> expr <*> loc comparison <*> expr
            <|> try nullNotNull
  where nullNotNull = expr >>= \e ->
          try (PNull <$> keywords ["is", "null"] <*> pure e
               <|> PNotNull <$> keywords ["is", "not", "null"] <*> pure e)

quoted :: Char -> Char -> P String
quoted open close =
  char open *> many quotedChar <* char close
  where quotedChar = satisfy (/= close)
                     <|> try (char close >> char close) 

-- xyz, [xyz], or "xyz"
rawIdent :: P String
rawIdent = quoted '[' ']' <|> quoted '"' '"' <|> identifier

ident :: P Ident
ident = Ident <$> loc rawIdent
        <?> "identifier"

-- TODO: Dotted notation (figure out least ugly way)
objectName :: P ObjectName
objectName = ObjectName Nothing Nothing Nothing
             <$> loc rawIdent
             <?> "object name"

tableRef :: P TableRef
tableRef = uncurry TableRef <$> withAlias objectName

-- Parses "x as y" or "x y"
withAlias :: P a -> P (a, Maybe Ident)
withAlias p = (,) <$> p <*> optionMaybe (optional (reserved "as") *> ident) 

select :: P QuerySpec
select = do
  reserved "select"
  top <- optionMaybe (reserved "top" *> topSpec)
  cols <- loc col `sepBy1` comma
  into <- optionMaybe (reserved "into" *> loc selectInto)
  from <- optionMaybe (reserved "from" *> loc tableRef)
  where' <- optionMaybe (reserved "where" *> loc cond)
  groupBy <- option [] $ keywords ["group", "by"] *> (expr `sepBy` comma)
  having <- optionMaybe (reserved "having" *> loc cond)
  return $ QuerySpec { qsTop = top
                     , qsColumns = cols
                     , qsInto = into
                     , qsFrom = from
                     , qsWhere = where'
                     , qsGroupBy = groupBy
                     , qsHaving = having }

  where
    col = try (SelectWildcard <$> optionMaybe (ident <* period) <* star)
          <|> uncurry SelectExpr <$> withAlias expr
          <?> "column specification"
    
    topSpec :: P (Located QueryTop)
    topSpec = loc $ Top
              <$> expr -- TODO number <|> parens expr
              <*> optionMaybe (keyword "percent")
              <*> optionMaybe (keywords ["with", "ties"])

    selectInto :: P SelectInto
    selectInto = SelectInto
                 <$> objectName
                 <*> optionMaybe (parens $ many1 ident)

declare :: P Statement
declare = Declare <$> keyword "declare" <*> (loc eqn `sepBy1` comma)
  where eqn :: P Decl
        eqn = Decl
              <$> (Ident <$> loc var)
              <*> loc typeName
              <*> optionMaybe (equals *> expr)
              <?> "declaration"

dropStatement :: P Statement
dropStatement = keyword "drop" *> (keyword "procedure" *> (DropProcedure <$> loc objectName))

createStatement :: P Statement
createStatement = keyword "create" *> (keyword "procedure" *> createProc <* eof)
  where createProc = CreateProcedure
                     <$> loc objectName
                     <*> (parens args <|> args)
                     <*> option (ProcedureOptions Nothing Nothing Nothing) options
                     <*> (reserved "as" *> statements)

        options :: P ProcedureOptions
        options = reserved "with" *>
                  permute (ProcedureOptions
                           <$?> (Nothing, Just <$> loc (keyword "recompile"))
                           <|?> (Nothing, Just <$> loc (keyword "encryption"))
                           <|?> (Nothing, keywords ["execute", "as"] *> (Just <$> loc (quoted '\'' '\'')))
                          )
                  <?> "'with' clause"

        args = arg `sepBy` comma
        arg = Argument
              <$> (Ident <$> loc var)
              <*> loc typeName
              <*> loc argType
              <*> optionMaybe (equals *> expr)

        argType = reserved "output" *> pure Output
                  <|> pure Input

flowStatement :: P Statement
flowStatement = block <|> if' <|> while <|> break <|> continue <|> goto <|> return
  where block = reserved "begin" *> (Block <$> statements) <* reserved "end"
        if' = reserved "if" *> (If <$> loc cond <*> loc statement <*> loc statement)
        while = reserved "while" *> (While <$> loc cond <*> loc statement)
        break = reserved "break" *> pure Break
        continue = reserved "continue" *> pure Continue
        return = reserved "return" *> (Return <$> expr)
        goto = reserved "goto" *> (Goto <$> (Ident <$> loc identifier))

dmlStatement :: P Statement
dmlStatement = declare
            <|> (Select <$> loc select)

ddlStatement :: P Statement
ddlStatement = dropStatement <|> createStatement

statement :: P Statement
statement = flowStatement <|> ddlStatement <|> dmlStatement <?> "statement"

statements :: P [Located Statement]
statements = loc statement `sepBy` optional semicolon

batch :: P [Located Statement]
batch = statements

withInput :: String -> P a -> P a
withInput s p = do
  i <- getInput
  setInput s
  x <- p
  setInput i
  return x

withPosition :: SourcePos -> P a -> P a
withPosition newPos p = do
  oldPos <- getPosition
  setPosition newPos
  x <- p
  setPosition oldPos
  return x
  
-- Parses a file in batch mode. If a line is "go", it will 
file = mapM parseChunk =<< splitWhen (isGo . map toLower . snd) <$> lines'
  where parseChunk [] = return []
        parseChunk ls@((pos,_):_) = withInput (unlines (map snd ls)) $ withPosition pos $ (whiteSpace *> statements <* eof)

        lines' = line `sepBy` char '\n'
        line = do p <- getPosition
                  l <- concat <$> many charToken
                  return (p, l)
        charToken = try lineComment <|> try blockComment <|> normalChar
        normalChar = satisfy (/= '\n') >>= \c -> return [c]

        -- Used to return blank here, but actually need to return the original comment (including embedded newlines) for the original error line numbers to be correct
        lineComment = string "--" >> ("--" ++) <$> many (satisfy (/= '\n')) -- >> return ""
        blockComment = startBlock >> (("/*" ++) . (++ "*/") <$> manyTill anyChar (try endBlock)) -- >> return ""
        startBlock = string "/*"
        endBlock = string "*/"
        
-- Is this line a "GO"? Don't count leading and trailing whitespace
isGo "" = False
isGo (c:xs) | isSpace c = isGo xs
isGo ('g':'o':xs) = all isSpace xs
isGo _ = False
                   
run :: P a -> SourceName -> String -> Either ParseError a
run p name str = runParser p () name str

checkFile path = do
  text <- readFile path
  return $ run file path text

checkAll path = do
  names <- D.getDirectoryContents path
  forM names $ \name -> do
    sql <- checkFile (path </> name)
    case sql of
     Left e -> return ()
     Right r -> putStrLn name >> print r
     
    
