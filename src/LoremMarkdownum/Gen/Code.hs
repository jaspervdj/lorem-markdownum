--------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module LoremMarkdownum.Gen.Code
    ( CodeConfig (..)
    , CodeGen
    , runCodeGen

    , Code

    , genCode
    , genStatement
    , genIdentifier
    , genBinOp

    , printCode
    , printStatement
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative        (Applicative, (<$>), (<*>))
import           Control.Monad              (replicateM)
import           Control.Monad.Reader       (MonadReader, ReaderT, ask,
                                             runReaderT)
import           Data.List                  (intersperse)
import           Data.Text                  (Text)


--------------------------------------------------------------------------------
import           LoremMarkdownum.FrequencyTree         (FrequencyTree)
import           LoremMarkdownum.Text.Util
import           LoremMarkdownum.Gen
import           LoremMarkdownum.Print


--------------------------------------------------------------------------------
data CodeConfig = CodeConfig
    { ccIdentifierSamples :: FrequencyTree Text
    } deriving (Show)


--------------------------------------------------------------------------------
type CodeGen m a = ReaderT CodeConfig m a


--------------------------------------------------------------------------------
runCodeGen :: MonadGen m => CodeGen m a -> CodeConfig -> m a
runCodeGen cg = runReaderT cg


--------------------------------------------------------------------------------
type Code = [Statement]


--------------------------------------------------------------------------------
data Statement
    = AssignmentS Lhs Expression
    | DeclarationS Identifier Expression
    | DestructiveUpdateS Lhs UpdateOp Expression
    | IfS Condition Code
    | IfElseS Condition Code Code
    | CallS Call
    deriving (Eq, Ord, Show)


--------------------------------------------------------------------------------
data Expression
    = BinOpE Expression BinOp Expression
    | ComparisonE Expression ComparisonOp Expression
    | IdentifierE Identifier
    | LiteralE Literal
    | CallE Call
    deriving (Eq, Ord, Show)


--------------------------------------------------------------------------------
type Condition = Expression


--------------------------------------------------------------------------------
newtype Identifier = Identifier Text deriving (Eq, Ord, Show)


--------------------------------------------------------------------------------
data BinOp = Add | Subtract | Multiply | Divide deriving (Eq, Ord, Show)


--------------------------------------------------------------------------------
data ComparisonOp = EqO | NeqO | GtO | GteO | StO | SteO
    deriving (Eq, Ord, Show)


--------------------------------------------------------------------------------
newtype UpdateOp = UpdateOp BinOp deriving (Eq, Ord, Show)


--------------------------------------------------------------------------------
data Literal = IntegerL Int deriving (Eq, Ord, Show)


--------------------------------------------------------------------------------
data Lhs
    = IdentifierLhs Identifier
    | FieldLhs Identifier Identifier
    deriving (Eq, Ord, Show)


--------------------------------------------------------------------------------
data Call = Call (Maybe Lhs) Identifier [Expression]
    deriving (Eq, Ord, Show)


--------------------------------------------------------------------------------
genCode :: MonadGen m => CodeGen m Code
genCode = do
    -- TODO (jaspervdj): Make this dependent on depth
    d             <- depth
    numStatements <- if d <= 0 then randomInt (3, 5) else randomInt (1, 3)
    replicateM numStatements genStatement


--------------------------------------------------------------------------------
genStatement :: MonadGen m => CodeGen m Statement
genStatement = depth >>= \d -> oneOfFrequencies
    [ (1, AssignmentS <$> genLhs <*> genExpression)
    , (if d <= 0 then 1 else 0,
        DeclarationS <$> genIdentifier <*> genExpression)
    , (1, DestructiveUpdateS <$> genLhs <*> genUpdateOp <*> genExpression)
    , (if d <= 0 then 1 else 0, IfS <$> genCondition <*> deeper genCode)
    , (if d <= 0 then 1 else 0,
        IfElseS <$> deeper genCondition <*> deeper genCode <*> deeper genCode)
    , (1, CallS <$> deeper genCall)
    ]


--------------------------------------------------------------------------------
genExpression :: MonadGen m => CodeGen m Expression
genExpression = depth >>= \d -> oneOfFrequencies
    [ (max 0 (3 - d),
        BinOpE <$> deeper genExpression <*> genBinOp <*> deeper genExpression)
    , (3, IdentifierE <$> genIdentifier)
    , (1, LiteralE <$> genLiteral)
    , (max 0 (3 - d), CallE <$> deeper genCall)
    ]


--------------------------------------------------------------------------------
genCondition :: MonadGen m => CodeGen m Condition
genCondition = oneOfFrequencies
    [ (2, genExpression)
    , (1, ComparisonE <$>
        deeper genExpression <*> genComparisonOp <*> deeper genExpression)
    ]


--------------------------------------------------------------------------------
genIdentifier :: MonadGen m => CodeGen m Identifier
genIdentifier = do
    ft   <- ccIdentifierSamples <$> ask
    len  <- randomInt (1, 3)
    join <- sampleFromList [camelCase, snakeCase]
    fmap (Identifier . join) $
        replicateM len $ sampleFromFrequencyTree ft


--------------------------------------------------------------------------------
genBinOp :: MonadGen m => CodeGen m BinOp
genBinOp = sampleFromFrequencies
    [(Add, 8), (Subtract, 3), (Multiply, 2), (Divide, 1)]


--------------------------------------------------------------------------------
genComparisonOp :: MonadGen m => CodeGen m ComparisonOp
genComparisonOp = sampleFromList [EqO, NeqO, GtO, GteO, StO, SteO]


--------------------------------------------------------------------------------
genUpdateOp :: MonadGen m => CodeGen m UpdateOp
genUpdateOp = UpdateOp <$> genBinOp


--------------------------------------------------------------------------------
genLiteral :: MonadGen m => CodeGen m Literal
genLiteral = IntegerL <$> genIntegerL
  where
    genIntegerL = oneOfFrequencies
        [ (6, randomInt (1, 5))
        , (2, randomInt (6, 100))
        , (2, randomInt (-5, -1))
        , (1, randomInt (100, 1000000))
        ]


--------------------------------------------------------------------------------
genLhs :: MonadGen m => CodeGen m Lhs
genLhs = oneOfFrequencies
    [(3, IdentifierLhs <$> genIdentifier), (1, genFieldLhs)]
  where
    genFieldLhs = FieldLhs <$> genIdentifier <*> genIdentifier


--------------------------------------------------------------------------------
genCall :: MonadGen m => CodeGen m Call
genCall = do
    hasLhs  <- randomBool 1 2
    mLhs    <- if hasLhs then Just <$> genLhs else return Nothing
    name    <- genIdentifier
    numArgs <- randomInt (1, 3)
    args    <- replicateM numArgs genExpression
    return $ Call mLhs name args


--------------------------------------------------------------------------------
printCode :: Code -> Print ()
printCode = printWrapIndent8 . mapM_ printStatement


--------------------------------------------------------------------------------
printStatement :: Statement -> Print ()
printStatement (AssignmentS lhs e) =
    printLhs lhs >> printBrkSp >> printText "=" >> printBrkSp >>
    printExpression e >> printText ";" >> printNl
printStatement (DeclarationS i e) =
    printText "var" >> printBrkSp >> printIdentifier i >> printBrkSp >>
    printText "=" >> printBrkSp >> printExpression e >> printText ";" >> printNl
printStatement (DestructiveUpdateS lhs o e) =
    printLhs lhs >> printBrkSp >> printUpdateOp o >> printBrkSp >>
    printExpression e >> printText ";" >> printNl
printStatement (IfS c code)  =
    printText "if (" >> printBrk >> printExpression c >> printText ") {" >>
    printNl >> printIndent4 (printCode code) >> printText "}" >> printNl
printStatement (IfElseS c code1 code2)  =
    printText "if (" >> printBrk >> printExpression c >> printText ") {" >>
    printNl >> printIndent4 (printCode code1) >> printText "} else {" >>
    printNl >> printIndent4 (printCode code2) >> printText "}" >> printNl
printStatement (CallS c) = printCall c >> printText ";" >> printNl


--------------------------------------------------------------------------------
printExpression :: Expression -> Print ()
printExpression (BinOpE l b r)  =
    printExpression l >> printBrkSp >> printBinOp b >>
    printBrkSp >> printExpression r
printExpression (ComparisonE l c r)  =
    printExpression l >> printBrkSp >> printComparisonOp c >>
    printBrkSp >> printExpression r
printExpression (IdentifierE i) = printIdentifier i
printExpression (LiteralE l)    = printLiteral l
printExpression (CallE c)       = printCall c


--------------------------------------------------------------------------------
printIdentifier :: Identifier -> Print ()
printIdentifier (Identifier txt) = printText txt


--------------------------------------------------------------------------------
printBinOp :: BinOp -> Print ()
printBinOp Add      = printText "+"
printBinOp Subtract = printText "-"
printBinOp Multiply = printText "*"
printBinOp Divide   = printText "/"


--------------------------------------------------------------------------------
printComparisonOp :: ComparisonOp -> Print ()
printComparisonOp EqO  = printText "=="
printComparisonOp NeqO = printText "!="
printComparisonOp GtO  = printText ">"
printComparisonOp GteO = printText ">="
printComparisonOp StO  = printText "<"
printComparisonOp SteO = printText "<="


--------------------------------------------------------------------------------
printUpdateOp :: UpdateOp -> Print ()
printUpdateOp (UpdateOp b) = printBinOp b >> printText "="


--------------------------------------------------------------------------------
printLiteral :: Literal -> Print ()
printLiteral (IntegerL n) = printShow n


--------------------------------------------------------------------------------
printLhs :: Lhs -> Print ()
printLhs (IdentifierLhs i) = printIdentifier i
printLhs (FieldLhs o i)    =
    printIdentifier o >> printText "." >> printIdentifier i


--------------------------------------------------------------------------------
printCall :: Call -> Print ()
printCall (Call mLhs name args) = do
    case mLhs of
        Nothing  -> return ()
        Just lhs -> printLhs lhs >> printText "."
    printIdentifier name >> printText "(" >> printBrk
    sequence_ $
        intersperse (printText "," >> printBrkSp) $ map printExpression args
    printText ")"
