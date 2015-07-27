{-# LANGUAGE LambdaCase #-}

module Language.TransactSql.Parser
       ( check
       , select
       , statement, statements
       , batch, batches
       ) where

import Text.Parsec hiding ((<|>), many)
import Text.Parsec.Combinator
import Text.Parsec.Expr
import Text.Parsec.Perm (permute, (<$?>), (<|?>))
import Text.Parsec.Char (alphaNum)
import Text.Parsec.Pos
import qualified Text.Parsec.Token as T

import Language.TransactSql.Types
import Language.TransactSql.AST
import Language.TransactSql.Loc

import Control.Applicative ((<$>), (<|>), (<*), (<*>), (*>), (<$), many, pure)
import Control.Monad (forM, forM_, liftM2, mzero, when, join)
import Data.Char (toLower, toUpper, isSpace, digitToInt)
import Data.Function (on)
import Data.List (foldl', isSuffixOf, sort)
import Data.Maybe (fromMaybe, catMaybes)
import Data.List.Split (splitWhen)
import System.Directory (getDirectoryContents)
import System.FilePath ((</>))

-- Comments probably need to be categorised:
-- 1. Whole line comments (whether block or line comments)
--    These probably reference the construct after the comment.
-- 2. End-of-line comments.
--    These probably reference the largest construct which is entirely
--    contained within the line.
-- 3. Inline comments.
--    It's hard to tell whether these reference the preceding or next construct,
--    or what level of a nested expression it might refer to.
--    E.g. declare @x int = @y + @z * 9.81 /* accel due to gravity */
--    Does the comment refer to 9.81? @z * 9.81? @y + @z * 9.81?
--    In this case it's probably best to attach it to the closest (and
--    smallest) possible construct.
data ParserState
  = ParserState { stBatchSeparator :: Maybe String
                , stUnclaimedComments :: [(SrcSpan, String)]
                }
  deriving (Eq, Ord, Show)

initialParserState = ParserState (Just "go") []
                   
type P a = Parsec String ParserState a

reservedWords = ["add", "all", "alter", "and", "any", "as", "asc", "authorization", "backup", "begin",
                 "between", "break", "browse", "bulk", "by", "cascade", "case", "check", "checkpoint",
                 "close", "clustered", "coalesce", "collate", "column", "commit", "compute", "constraint",
                 "contains", "containstable", "continue", "convert", "create", "cross", "current",
                 "current_date", "current_time", "current_timestamp", "current_user", "cursor", "database",
                 "dbcc", "deallocate", "declare", "default", "delete", "deny", "desc", "disk", "distinct",
                 "distributed", "double", "drop", "dump", "else", "end", "errlvl", "escape", "except",
                 "exec", "execute", "exists", "exit", "external", "fetch", "file", "fillfactor", "for",
                 "foreign", "freetext", "freetexttable", "from", "full", "function", "goto", "grant",
                 "group", "having", "holdlock", "identity", "identity_insert", "identitycol", "if", "in",
                 "index", "inner", "insert", "intersect", "into", "is", "join", "key", "kill", "left",
                 "like", "lineno", "load", "merge", "national", "nocheck", "nonclustered", "not", "null",
                 "nullif", "of", "off", "offsets", "on", "open", "opendatasource", "openquery", "openrowset",
                 "openxml", "option", "or", "order", "outer", "over", "percent", "pivot", "plan", "precision",
                 "primary", "print", "proc", "procedure", "public", "raiserror", "read", "readtext", "reconfigure",
                 "references", "replication", "restore", "restrict", "return", "revert", "revoke", "right",
                 "rollback", "rowcount", "rowguidcol", "rule", "save", "schema", "securityaudit", "select",
                 "semantickeyphrasetable", "semanticsimilaritydetailstable", "semanticsimilaritytable",
                 "session_user", "set", "setuser", "shutdown", "some", "statistics", "system_user", "table",
                 "tablesample", "textsize", "then", "to", "top", "tran", "transaction", "trigger", "truncate",
                 "try_convert", "tsequal", "union", "unique", "unpivot", "update", "updatetext", "use", "user",
                 "values", "varying", "view", "waitfor", "when", "where", "while", "with", "within group", "writetext"]

operatorChars = "-=+*&^%/<>~|."

comment = try (storeComment lineComment) <|> try (storeComment blockComment) <?> ""
  where lineComment = string "--" *> many (noneOf "\n") <?> ""
        blockComment = string "/*" *> inComment <* string "*/" <?> ""
        inComment = concat <$> many commentText <?> "end of comment"
        commentText = (:[]) <$> noneOf "*/"
                      <|> try (string "/*" *> nestedComment)
                      <|> try (string "*" <* notFollowedBy (char '/'))
                      <|> string "/"
                      <?> "end of comment"
        nestedComment = concat <$> sequence [pure "/*", inComment, string "*/"]
                      
        storeComment :: P String -> P ()
        storeComment p = do pos <- getPosition
                            text <- p
                            pos' <- getPosition
                            modifyState $ \(ParserState bs cs) -> ParserState bs ((mkSpan pos pos', text):cs)


-- Matches semantic white space, including comments and newlines (but not where there's a batch separator!)
whiteSpace = skipMany space <?> ""
  where space = lineSpace <|> try newLine <|> comment <?> ""
        newLine = char '\n' *> notFollowedBy (batchSeparator *> (fromMaybe "go" . stBatchSeparator <$> getState)) <?> "" -- notFollowedBy calls "show" on whatever's returned

inlineWhiteSpace = skipMany space <?> ""
  where space = lineSpace <|> comment <?> ""

-- Batch separators can be followed by a number, which specifies how many
-- times the batch is to be run. Why, I have no idea...
batchSeparator :: P (Maybe (Located Integer))
batchSeparator =  startOfLine >> fmap stBatchSeparator getState >>= \case
  Just bs -> inlineWhiteSpace
             *> string bs
             *> inlineWhiteSpace
             *> optionMaybe (loc nat <* inlineWhiteSpace)
             <* (char '\n' *> return () <|> eof)
             <?> show bs
  Nothing -> fail "Cannot use batch separators inside a batch"
  where startOfLine = do col <- sourceColumn <$> getPosition
                         when (col /= 1) $ fail "Batch separator can't appear mid-line"

-- Parses a quoted string, where the only character that needs to be escaped is the
-- close character, which is escaped by writing it twice.
quoted :: Char -> Char -> P String
quoted open close =
  char open *> many quotedChar <* char close
  where quotedChar = satisfy (/= close)
                     <|> try (char close >> char close) 

-- Matches actual white space char within a particular line
lineSpace :: P ()
lineSpace = satisfy (\c -> isSpace c && c /= '\n') *> return ()

nat, natural :: P Integer
natural = lexeme nat
nat = foldl' (\x d -> 10*x + toInteger (digitToInt d)) 0 <$> many1 digit 

integer = lexeme sign <*> natural
  where sign = char '-' *> pure negate
               <|> char '+' *> pure id
               <|> pure id

lexeme p = p <* whiteSpace

reserved name = lexeme $ try $ ciString name <* (notFollowedBy (alphaNum <|> char '_') <?> "end of " ++ show name)
ciString name = walk name *> pure name
  where walk [] = return ()
        walk (c:cs) = (char (toLower c) <|> char (toUpper c) <?> show name) *> walk cs
        
identifier = lexeme $ try $ do name <- ident
                               if isReservedName name
                                 then unexpected ("reserved word " ++ show name)
                                 else return name
  where ident = (:) <$> (letter <|> char '_') <*> many (alphaNum <|> char '_')
                <?> "identifier"
                
isReservedName name = map toLower name `elem` reservedWords -- TODO: performance?

symbol name = lexeme (string name)
operator = try $ lexeme $ many1 (oneOf operatorChars)
reservedOp name = lexeme $ string name <* notFollowedBy (oneOf operatorChars <?> "end of " ++ show name)

parens p = between (symbol "(") (symbol ")") p
squares p = between (symbol "[") (symbol "]") p

boundedInt :: Integer -> Integer -> P Int
boundedInt low high = do
  n <- natural
  when (n > high || n < low) $
    fail ("expected number between " ++ show low ++ " and " ++ show high)
  return (fromIntegral n)

var = lexeme (char '@' *> identifier)
serverVar = lexeme (char '@' *> char '@' *> identifier)

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
keyword word = Keyword <$> loc (reserved word)

keywords :: [String] -> P Keyword
keywords words = Keyword <$> loc (mapM_ reserved words >> pure (unwords words))
                 <?> show (unwords words)

stringLit = lexeme $ (quoted '\'' '\'')

literal :: P Literal
literal = (IntL <$> loc integer)
          <|> (StrL <$> loc stringLit)
          <|> try (char 'N' *> (NStrL <$> loc stringLit))
          <|> (NullL <$> keyword "null")
          <?> "value"

typeName :: Int -> P SqlType
typeName defLen = (identifier >>= rest) <?> "type name"
  where rest "int" = pure Int
        rest "char" = Char <$> len
        rest "nchar" = NChar <$> len
        rest "varchar" = VarChar <$> len
        rest "nvarchar" = NVarChar <$> len
        rest "binary" = Binary <$> len
        rest "varbinary" = VarBinary <$> len
        rest "float" = Float <$> spec 1 64 53
        rest "real" = Float <$> pure 24
        rest "smalldatetime" = pure SmallDateTime
        rest "date" = pure Date
        rest "money" = pure Money
        rest "smallmoney" = pure SmallMoney
        rest "bit" = pure Bit
        rest "bigint" = pure BigInt
        rest "smallint" = pure SmallInt
        rest "tinyint" = pure TinyInt
        rest "datetime" = pure DateTime 
        rest "datetime2" = DateTime2 <$> spec 0 7 7
        rest "time" = Time <$> spec 0 7 7
        rest "datetimeoffset" = DateTimeOffset <$> spec 0 7 7
        rest "text" = pure Text
        rest "ntext" = pure NText
        rest "image" = pure Image
        rest name = fail $ "unknown type name: " ++ name

        spec minVal maxVal defVal = option defVal (parens $ boundedInt 0 maxVal)
        len = parens (reserved "max" *> pure (-1)
                      <|> boundedInt 1 0x7FFFFFFF)
              <|> pure defLen

castTypeName, declTypeName :: P SqlType
castTypeName = typeName 30
declTypeName = typeName 1

term :: P Expr
term = unLoc <$> parens expr
       <|> varName 
       <|> ELit <$> loc literal
       -- TODO: br.dateProcessed
       <|> try (EColRef . Just <$> (ident <* period) <*> ident)
       <|> reserved "cast" *> parens (ECast <$> expr <*> (reserved "as" *> loc castTypeName))
       <|> convert
       <|> caseExpr
       <|> try funCall
       <|> EColRef Nothing <$> ident
  where caseExpr = reserved "case" *> (condCase <|> exprCase) <* reserved "end"
        condCase = ESearchCase <$> many1 (branch (loc cond)) <*> final
        exprCase = ESimpleCase <$> expr <*> many1 (branch expr) <*> final
        branch on = (,) <$> (reserved "when" *> on) <*> (reserved "then" *> expr)
        final = optionMaybe (reserved "else" *> expr)
        varName = try (EServerVar <$> Ident <$> loc serverVar)
                  <|> EVar <$> Ident <$> loc var
                  <?> "variable name"
        convert = reserved "convert" *>
                  parens (EConvert <$> loc castTypeName
                                   <*> (comma *> expr)
                                   <*> optionMaybe (comma >> loc integer))
        funCall = EFunCall <$> try (specialFun <|> objectName) <*> parens (expr `sepBy` comma)
        specialFun = choice $ map (\w -> ObjectName Nothing Nothing Nothing <$> loc (reserved w)) ["left", "right"]

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

locChainl1 :: P a -> P (Located a -> Located a -> a) -> P a
locChainl1 p op = join (rest <$> getPosition <*> loc p)
  where rest startPos left = chained startPos left <|> pure (unLoc left)
        chained startPos left = do
          f <- op
          right <- loc p
          pos <- getPosition
          rest startPos $ L (mkSpan startPos pos) (f left right)
                  
cond :: P Cond
cond = orCond <?> "condition"
  where basicCond = CNot <$> keyword "not" <*> loc cond
                    <|> CPred <$> predicate
        andCond = locChainl1 basicCond (CAnd <$> keyword "and")
        orCond = locChainl1 andCond (COr <$> keyword "or")

comparison :: P Comparison
comparison = reservedOp "=" *> pure CEq
         <|> try (reservedOp "<>" *> pure CNeq)
         <|> try (reservedOp ">=" *> pure CGte)
         <|> try (reservedOp "<=" *> pure CLte)
         <|> reservedOp ">" *> pure CGt
         <|> reservedOp "<" *> pure CLt
         <?> "comparison operator"

predicate :: P Pred
predicate = PExists <$> keyword "exists" <*> parens (loc select)
            <|> (expr >>= expressionPred)
  where expressionPred e =
          PCompare <$> (loc comparison) <*> pure e <*> expr
          <|> PNull <$> keywords ["is", "null"] <*> pure e
          <|> PNotNull <$> keywords ["is", "not", "null"] <*> pure e
          <|> PBetween <$> keyword "between" <*> pure e <*> expr <*> expr

-- xyz, [xyz], or "xyz"
rawIdent :: P String
rawIdent = quoted '[' ']' <|> quoted '"' '"' <|> identifier

ident :: P Ident
ident = Ident <$> loc rawIdent
        <?> "identifier"

-- TODO: Dotted notation (figure out least ugly way)
objectName :: P ObjectName
objectName = try (ObjectName <$> part
                             <*> (period *> opt)
                             <*> (period *> opt)
                             <*> (period *> loc rawIdent)) -- server.database?.schema?.obj
             <|> try (ObjectName Nothing <$> part
                                         <*> (period *> opt)
                                         <*> (period *> loc rawIdent)) -- database.schema?.obj
             <|> try (ObjectName Nothing Nothing <$> part <*> (period *> loc rawIdent))
             <|> ObjectName Nothing Nothing Nothing <$> loc rawIdent
             <?> "object name"
  where part = Just <$> loc rawIdent
        opt = optionMaybe (loc rawIdent)

basicTableRef, tableRef :: P TableRef
tableRef = join $ rest <$> getPosition <*> loc basicTableRef
  where rest startPos left =
          joinWithCond startPos left
          <|> joinNoCond startPos left
          <|> pure (unLoc left)

        -- inner join, left join, right join, full outer join...
        joinWithCond startPos left = do
          f <- opWithCond
          right <- loc basicTableRef
          cond <- loc joinCondition
          pos <- getPosition
          let left' = L (mkSpan startPos pos) (f left right (Just cond))
          rest startPos left'

        -- cross join, cross apply, etc?
        joinNoCond startPos left = do
          f <- opNoCond
          right <- loc basicTableRef
          pos <- getPosition
          let left' = L (mkSpan startPos pos) (f left right Nothing)
          rest startPos left'

        joinCondition = reserved "on" *> cond

        opWithCond, opNoCond :: P (Located TableRef -> Located TableRef -> Maybe (Located Cond) -> TableRef)
        opWithCond = Join <$> loc (Inner <$ (optional (reserved "inner") *> reserved "join"))
                     <|> Join <$> loc (RightOuter <$ (reserved "right" *> reserved "join"))
                     <|> Join <$> loc (LeftOuter <$ (reserved "left" *> optional (reserved "outer") *> reserved "join"))
                     <|> Join <$> loc (FullOuter <$ (reserved "full" *> optional (reserved "outer") *> reserved "join"))
                     <?> "join operator"
                     
        opNoCond = Join <$> loc (Cross <$ (reserved "cross" *> reserved "join"))
                   <?> "join operator"

basicTableRef = uncurry TableRef <$> withAlias objectName <?> "table name or subquery"

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
          <|> try (SelectIntoVar <$> (Ident <$> loc var) <*> (symbol "=" *> expr))
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
              <*> (optional (reserved "as") *> loc declTypeName)
              <*> optionMaybe (equals *> expr)
              <?> "declaration"

dropStatement :: P Statement
dropStatement = keyword "drop" *> (keyword "procedure" *> (DropProcedure <$> loc objectName))

createStatement :: P Statement
createStatement = keyword "create" *> (keyword "procedure" *> createProc)
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
              <*> (optional (reserved "as") *> loc declTypeName)
              <*> loc argType
              <*> optionMaybe (equals *> expr)

        argType = reserved "output" *> pure Output
                  <|> pure Input

flowStatement :: P Statement
flowStatement = block <|> if' <|> while <|> break <|> continue <|> goto <|> return
  where block = reserved "begin" *> (Block <$> statements) <* reserved "end"
        if' = reserved "if" *> (If <$> loc cond
                                   <*> loc statement
                                   <*> optionMaybe (reserved "else" *> loc statement))
        while = reserved "while" *> (While <$> loc cond <*> loc statement)
        break = reserved "break" *> pure Break
        continue = reserved "continue" *> pure Continue
        return = reserved "return" *> (Return <$> expr)
        goto = reserved "goto" *> (Goto <$> (Ident <$> loc identifier))

dmlStatement :: P Statement
dmlStatement = declare
            <|> (Select <$> loc select)
            <|> reserved "insert" *> (optional $ reserved "into") *> insert
            <|> reserved "set" *> (SetVar <$> (Ident <$> loc var) <*> (equals *> expr))
  where insert = Insert <$> objectName
                        <*> option [] (parens $ ident `sepBy1` comma)
                        <*> insertFrom

        insertFrom = Nothing <$ keywords ["default", "values"]
                     <|> Just . Subquery <$> loc select
                     <|> Just . ValueList <$> values

values = reserved "values" *> (parens row `sepBy1` comma)
  where row = expr `sepBy1` comma

ddlStatement :: P Statement
ddlStatement = dropStatement <|> createStatement

execStatement :: P Statement
execStatement = do (keyword "exec" <|> keyword "execute") *> (execString <|> execModule)
  where execString = ExecString <$> parens (loc (ELit <$> loc (StrL <$> loc stringLit))) -- TODO: concatenation here
        execModule = do
          target <- optionMaybe $ try $ (Ident <$> loc var) <* equals
          mod <- Left <$> objectName
                 <|> Right . Ident <$> loc var
          posArgs <- loc posArg `sepBy` comma
          namedArgs <- if null posArgs -- Don't need a comma if there were no positional arguments
                         then namedArg `sepBy` comma
                         else option [] (comma *> (namedArg `sepBy` comma))
          return $ Exec target mod posArgs namedArgs

        posArg = ELit <$> loc literal
                 <|> EVar . Ident <$> loc var
                 <|> reserved "default" *> fail "Not implemented" -- TODO: implement (add to AST?)

        namedArg = (,) <$> (Ident <$> (loc var <* equals)) <*> loc posArg

backupStatement :: P Statement
backupStatement = Backup <$> (reserved "backup" *> loc backupType)
                         <*> what
                         <*> (reserved "to" *> target)
  where backupType = DatabaseBackup <$ reserved "database"
                     <|> LogBackup <$ reserved "log"
        what = BackupStatic <$> ident
               <|> BackupDynamic <$> (Ident <$> loc var)
        target = BackupToDisk <$> (reserved "disk" *> equals *> loc stringLit)
                 <|> BackupToDevice <$> ident
        

statement :: P Statement
statement = execStatement
            <|> flowStatement
            <|> ddlStatement
            <|> dmlStatement
            <|> backupStatement
            <?> "statement"

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

batches = do
  b <- batch
  more b <|> (return [Batch b Nothing])
  where more b = do reps <- findBatchSeparator
                    (Batch b reps:) <$> batches
                  

-- The whiteSpace combination will refuse to read a line if finds a batchSeparator, so that
-- we know we won't find a batchSeparator part-way through a line.
-- Hence, here we can just attempt to read any empty lines (which there may or not be
-- depending on if this follows a lexeme) and check for a batchSeparator.
findBatchSeparator = lexeme $ try (emptyLines >> batchSeparator)
  where emptyLines = skipMany (try (inlineWhiteSpace >> char '\n'))
                   
check :: P a -> String -> Either ParseError a
check p str = runParser p initialParserState "input" str

checkFile path = runParser (whiteSpace *> batches <* whiteSpace <* eof) initialParserState path <$> readFile path

checkAll path = do
  names <- filter (".sql" `isSuffixOf`) . sort <$> getDirectoryContents path
  passed <- forM names $ \name -> do
    sql <- checkFile (path </> name)
    case sql of
     Left e -> do
       putStrLn $ take 120 (concat (lines (show e)))
       return Nothing -- fail (show e)
     Right r -> do
       writeFile (path </> (name ++ ".ast")) (show r)
       return (Just name)
  putStrLn "--------"
  forM_ (zip [1..] (catMaybes passed)) $ \(i,name) -> do
    putStrLn $ (show i) ++ ". " ++ name
  putStrLn $ show (length (catMaybes passed)) ++ " out of " ++ show (length passed)
     
    
