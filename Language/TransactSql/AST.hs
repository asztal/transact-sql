{-# LANGUAGE DeriveDataTypeable #-}

module Language.TransactSql.AST
       ( Literal(..)
       , Ident(..)
       , ObjectName(..)
       , Tag(..)
       , Keyword(..)
       , UnaryOp(..)
       , BinaryOp(..)
       , Comparison(..)
       , SubqueryOp(..)
       , SubqueryQualifier(..)
       , AggregateMode(..)
       , Expr(..)
       , Subquery(..)
       , Qualifier(..)
       , Cond(..)
       , Pred(..)
       , QueryTop(..)
       , SelectInto(..)
       , SelectColumn(..)
       , QuerySpec(..)
       , Order(..)
       , SetOp(..)
       , QueryExpression(..)
       , JoinType(..)
       , TableRef(..)
       , Condition(..)
       , Decl(..)
       , Statement(..)
       , Argument(..)
       , ArgumentType(..)
       , ProcedureOptions(..)
       , Batch(..)
       , CursorOptions(..)
       , BackupType(..)
       , BackupSource(..)
       , BackupDestination(..)
       , FetchType(..)
       ) where

import Data.Generics

import Language.TransactSql.Types
import Language.TransactSql.Tag
import Language.TransactSql.Loc

-- Tagged syntax tree. Tag can be anything but is usually a LocTag.
data Literal
    = IntL (Located Integer)
    | NumericL (Located Rational) Precision Scale
    | StrL (Located String)
    | NStrL (Located String)
    | BinL (Located String) -- TODO BS.Word8?
    | NullL Keyword
    deriving (Eq, Ord, Show, Data, Typeable)

newtype Ident = Ident (Located String) deriving (Show, Eq, Ord, Data, Typeable)

data ObjectName
  = ObjectName
    { onServer :: Maybe (Located String)
    , onDatabase :: Maybe (Located String)
    , onSchema :: Maybe (Located String)
    , onName :: Located String
    }
  deriving (Eq, Ord, Data, Typeable)

instance Show ObjectName where
  show (ObjectName srv db scm (L _ name)) =
    addDot srv ++ addDot db ++ addDot scm ++ name
    where addDot (Just (L _ x)) = x ++ "."
          addDot Nothing = ""

data Keyword = Keyword (Located String)
             deriving (Eq, Ord, Show, Data, Typeable)

data UnaryOp
  = UNegate -- i.e. -
  | UInvert -- i.e. ~
  | UPlus
  deriving (Eq, Ord, Show, Read, Data, Typeable)

data BinaryOp
  = BAdd
  | BSub
  | BMul
  | BDiv
  | BMod
  | BAnd
  | BOr
  | BXor
  deriving (Eq, Ord, Show, Read, Data, Typeable)

data Comparison
  = CEq
  | CNeq
  | CGt
  | CGte
  | CLt
  | CLte
  deriving (Eq, Ord, Show, Read, Data, Typeable)

data SubqueryOp
  = SIn
  | SNotIn
  | SEqAny
  deriving (Eq, Ord, Show, Read, Data, Typeable)

data SubqueryQualifier
  = SAny
  | SSome
  | SAll
  deriving (Eq, Ord, Show, Read, Data, Typeable)

data AggregateMode = AggregateAll | AggregateDistinct
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Data, Typeable)

data Expr
  = EVar Ident
  | EServerVar Ident
  | ELit (Located Literal)
  | EColRef { crSource :: Maybe Ident, crName :: Ident }
  | EUnary (Located UnaryOp) (Located Expr)
  | EBin (Located BinaryOp) (Located Expr) (Located Expr)
  | EWindowed 
    { wfFun :: Ident
    , wfMode :: Located AggregateMode
    , wfExpr ::  Maybe (Located Expr)
    , wfPartition :: [Located Expr]
    , wfOrder :: [(Located Expr, Located Order)] }
    -- TODO rows 
  | EFunCall ObjectName [Located Expr]
  | ECoalesce [Located Expr]
  | ESearchCase [(Located Cond, Located Expr)] (Maybe (Located Expr))
  | ESimpleCase (Located Expr) [(Located Expr, Located Expr)] (Maybe (Located Expr))
  | ECast (Located Expr) (Located SqlType)
  | EConvert (Located SqlType) (Located Expr) (Maybe (Located Integer))
  | EScalarSubquery (Located QuerySpec)
  deriving (Eq, Ord, Data, Typeable)

instance Show Expr where
  showsPrec d (EVar (Ident (L _ s))) str
    = '@' : (s ++ str)
  showsPrec d (EServerVar (Ident (L _ s))) str
    = '@' : '@' : (s ++ str)
  showsPrec d (ELit (L _ x)) str
    = showsPrec d x str
  showsPrec d (EColRef (Just (Ident (L _ src))) (Ident (L _ name))) str
    = ('[' :) . (src ++) . ("].[" ++) . (name ++) . (']' :) $ str
  showsPrec d (EColRef Nothing (Ident (L _ name))) str
    = ('[' :) . (name ++) . (']' :) $ str
  showsPrec d (EUnary (L _ op) (L _ expr)) str
    = showsPrec d op $ showsPrec d expr str
  showsPrec d (EBin (L _ op) (L _ left) (L _ right)) str
    = ('(':) . showsPrec d left . (" `" ++) . showsPrec d op . ("` " ++) . showsPrec d right . (')':) $ str
  showsPrec d (EWindowed (Ident (L _ f)) m x ps os) str = (f ++) . ("(" ++) . showsPrec d m . (" " ++) . showsPrec d x . (") over (partition by " ++) . showList ps . (" order by " ++) . showList os . (")" ++) $ str
  showsPrec d (EFunCall f xs) str = showsPrec d f . ("(" ++) . showList xs . (")" ++) $ str
  showsPrec d (ECoalesce xs) str = ("coalesce(" ++) . showList xs . (")" ++) $ str
  showsPrec d (ECast (L _ expr) (L _ ty)) str
    = ("cast(" ++) . showsPrec d expr . (" as " ++) . showsPrec 0 ty . (')' :) $ str
  showsPrec d (EConvert (L _ ty) (L _ expr) x) str
    = ("convert(" ++) . showsPrec d ty . (", " ++) . showsPrec 0 expr . (", " ++) . showsPrec 0 x . (')' :) $ str
  showsPrec d (ESimpleCase (L _ e) cs else') str = ("case " ++) . showsPrec d e . (' ':) . (show cs ++) . (' ':) . showsPrec d else' $ str
  showsPrec d (ESearchCase cs else') str = ("case " ++) . showsPrec 0 cs . (' ':) . showsPrec d else' $ str
  showsPrec _ x str = gshow x ++ str

data Subquery
  = Subquery (Located QuerySpec)
  | ValueList [[Located Expr]]
  deriving (Eq, Ord, Show, Data, Typeable)

data Qualifier = All | Some | Any
               deriving (Eq, Ord, Show, Read, Data, Typeable) 
  
data Cond
  = CPred Pred
  | CNot Keyword (Located Cond)
  | CAnd Keyword (Located Cond) (Located Cond)
  | COr Keyword (Located Cond) (Located Cond)
  deriving (Eq, Ord, Show, Data, Typeable)

data Pred
  = PCompare (Located Comparison) (Located Expr) (Located Expr)
  | PLike Keyword (Located Expr) (Located Expr) (Maybe (Located Char))
  | PNull Keyword (Located Expr)
  | PNotNull Keyword (Located Expr)
  | PBetween Keyword (Located Expr) (Located Expr) (Located Expr)
  | PNotBetween Keyword (Located Expr) (Located Expr) (Located Expr)
  | PContains Keyword (Maybe (Located Ident))
  | PFreeText Keyword (Located Expr)
  | PIn Keyword (Located Expr) (Located Subquery)
  | PNotIn Keyword (Located Expr) (Located Subquery)
  | PCompareSubquery (Located Comparison) (Maybe (Located Qualifier)) (Located Expr) (Located Subquery)
  | PExists Keyword (Located QuerySpec)
  deriving (Eq, Ord, Show, Data, Typeable)

data QueryTop
    = Top
      { topExpr :: Located Expr
      , topIsPercentage :: Maybe Keyword
      , topWithTies :: Maybe Keyword }
    deriving (Eq, Ord, Show, Data, Typeable)

data SelectInto = SelectInto ObjectName (Maybe [Ident])
                deriving (Eq, Ord, Show, Data, Typeable)

data SelectColumn = SelectExpr (Located Expr) (Maybe Ident)
                  | SelectIntoVar (Ident) (Located Expr)
                  | SelectWildcard (Maybe Ident)
                  deriving (Eq, Ord, Show, Data, Typeable)

data Order = Asc | Desc
           deriving (Eq, Ord, Show, Enum, Bounded, Data, Typeable)

data QuerySpec
    = QuerySpec
      { qsTop :: Maybe (Located QueryTop)
      , qsColumns :: [Located SelectColumn]
      , qsInto :: Maybe (Located SelectInto)
      , qsFrom :: Maybe (Located TableRef)
      , qsWhere :: Maybe (Located Cond)
      , qsGroupBy :: [Located Expr]
      , qsHaving :: Maybe (Located Cond)
      , qsOrderBy :: [(Located Expr, Located Order)] }
    deriving (Eq, Ord, Show, Data, Typeable)

data SetOp = Union | UnionAll | Except | Intersect
           deriving (Eq, Ord, Show, Read, Enum, Bounded, Data, Typeable)

data QueryExpression
    = QSelect (Located QuerySpec)
    | QSetOp (Located SetOp) (Located QueryExpression) (Located QueryExpression)
    deriving (Eq, Ord, Show)

data JoinType = Inner | LeftOuter | RightOuter | FullOuter | Cross
              deriving (Eq, Ord, Show, Read, Enum, Bounded, Data, Typeable)

data TableRef
    = TableRef
      { trTable :: ObjectName
      , trAlias :: Maybe Ident }
    | Join
      { tjType :: Located JoinType
      , tjLeft :: Located TableRef
      , tjRight :: Located TableRef
      , tjCond :: Maybe (Located Cond) }
    | QueryRef
      { qrQuery :: Subquery
      , qrAlias :: Maybe (Ident, [Ident]) }
    -- | CrossApply
    -- | OuterApply
    deriving (Eq, Ord, Show, Data, Typeable)

data Condition
    = Equal Keyword (Located Expr) (Located Expr)
    | In Keyword (Located Expr)
    | NotIn Keyword (Located Expr)
    deriving (Eq, Ord, Show, Data, Typeable)

data Decl = Decl Ident (Located SqlType) (Maybe (Located Expr))
          deriving (Eq, Ord, Show, Data, Typeable)

data ArgumentType = Input | Output
                  deriving (Eq, Ord, Show, Data, Typeable, Enum, Bounded)

data Argument = Argument Ident (Located SqlType) (Located ArgumentType) (Maybe (Located Expr))
                deriving (Eq, Ord, Show, Data, Typeable)

data ProcedureOptions = ProcedureOptions
                        { procRecompile :: Maybe (Located Keyword)
                        , procEncryption :: Maybe (Located Keyword)
                        , procExecuteAs :: Maybe (Located String)
                        }
                        deriving (Eq, Ord, Show, Data, Typeable)

data Statement
    = Declare Keyword [Located Decl]
    | Select (Located QuerySpec)
    | Insert ObjectName [Ident] (Maybe Subquery) -- Nothing = "default values"
    | SetVar Ident (Located Expr)
    | If (Located Cond) (Located Statement) (Maybe (Located Statement))
    | While (Located Cond) (Located Statement)
    | Break
    | Continue
    | Return (Maybe (Located Expr))
    | Goto Ident
    | Throw
    | ThrowSpecific { throwErrorNo :: Either (Located Int) Ident
                    , throwMessage :: Either (Located String) Ident
                    , throwState :: Either (Located Int) Ident }
    | Try (Located Statement) (Located Statement)
    | DropProcedure (Located ObjectName)
    | CreateProcedure (Located ObjectName) [Argument] ProcedureOptions [Located Statement]
    | Block [Located Statement]
    | ExecString (Located Expr) -- Can be concatted strings
    | Exec (Maybe Ident) (Either ObjectName Ident) [Located Expr] [(Ident, Located Expr)]
    | Backup (Located BackupType) BackupSource BackupDestination -- TODO: other stuff...
    | DeclareCursor Ident CursorOptions (Located QuerySpec)
    | Open Ident
    | Close Ident
    | Deallocate Ident
    | Fetch (Located FetchType) Ident [Ident]
    -- | Update | Delete
    -- | Drop | Create | Alter
    deriving (Eq, Ord, Show, Data, Typeable)

data CursorOptions = CursorOptions
                   deriving (Eq, Ord, Show, Data, Typeable)
data FetchType = FetchCurrent | FetchNext | FetchPrior | FetchFirst | FetchLast
                deriving (Eq, Ord, Show, Data, Typeable, Enum, Bounded)

data BackupType = DatabaseBackup | LogBackup
                deriving (Eq, Ord, Show, Data, Typeable, Enum, Bounded)

data BackupSource = BackupStatic Ident -- backup database [DATABASE_NAME]
                  | BackupDynamic Ident -- backup database @database_name_var
                  deriving (Eq, Ord, Show, Data, Typeable)

data BackupDestination = BackupToDisk (Located FilePath)
                       | BackupToDevice Ident
                deriving (Eq, Ord, Show, Data, Typeable)

data Batch = Batch [Located Statement] (Maybe (Located Integer))
             deriving (Eq, Ord, Show, Data, Typeable)
