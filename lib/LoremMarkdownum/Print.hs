--------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module LoremMarkdownum.Print
    ( PrintOptions (..)
    , defaultPrintOptions

    , Print
    , runPrint
    , runPrintWith
    , runPrintLength

    , printText
    , printShow

    , printBrkSp
    , printBrk
    , printNl

    , printIndent
    , printIndent4
    , printWrapIndent
    , printWrapIndent8
    ) where


--------------------------------------------------------------------------------
import           Control.Monad.Reader   (ask, local)
import           Control.Monad.RWS      (RWS, runRWS)
import           Control.Monad.State    (get, modify)
import           Control.Monad.Writer   (tell)
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Lazy         as TL
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as TLB


--------------------------------------------------------------------------------
data PrintOptions = PrintOptions
    { pcWrapCol :: Maybe Int
    } deriving (Show)


--------------------------------------------------------------------------------
defaultPrintOptions :: PrintOptions
defaultPrintOptions = PrintOptions
    { pcWrapCol = Just 80
    }


--------------------------------------------------------------------------------
data PrintRead = PrintRead
    { prMaxCol     :: Maybe Int
    , prIndent     :: Text
    , prWrapIndent :: Text
    } deriving (Show)


--------------------------------------------------------------------------------
data PrintState = PrintState
    { psCol      :: Int
    , psSpace    :: Bool
    , psQueue    :: (Builder, Int)
    , psWrapping :: Bool
    } deriving (Show)


--------------------------------------------------------------------------------
newtype Print a = Print {unPrint :: RWS PrintRead Builder PrintState a}
    deriving (Applicative, Functor, Monad)


--------------------------------------------------------------------------------
runPrint :: Print () -> TL.Text
runPrint = runPrintWith defaultPrintOptions


--------------------------------------------------------------------------------
runPrintWith :: PrintOptions -> Print () -> TL.Text
runPrintWith config pr =
    let (_, _, bld) = runRWS (unPrint (pr >> printFlush)) printRead printState
    in TLB.toLazyText bld
  where
    printRead  = PrintRead (pcWrapCol config) "" ""
    printState = PrintState 0 False (mempty, 0) False


--------------------------------------------------------------------------------
runPrintLength :: Print () -> Int
runPrintLength = fromIntegral . TL.length . runPrint


--------------------------------------------------------------------------------
printText :: Text -> Print ()
printText text = Print $ do
    (b, l) <- psQueue <$> get
    modify $ \s -> s {psQueue = (b <> TLB.fromText text, l + T.length text)}


--------------------------------------------------------------------------------
printShow :: Show a => a -> Print ()
printShow = printText . T.pack . show


--------------------------------------------------------------------------------
printBrkSp :: Print ()
printBrkSp = Print $ do
    unPrint printFlush
    modify $ \s -> s {psSpace = True}


--------------------------------------------------------------------------------
printBrk :: Print ()
printBrk = printFlush


--------------------------------------------------------------------------------
printNl :: Print ()
printNl = Print $ do
    unPrint printFlush
    modify $ \s -> s {psCol = 0, psWrapping = False}
    tell "\n"


--------------------------------------------------------------------------------
printFlush :: Print ()
printFlush = Print $ do
    mbMaxCol   <- prMaxCol   <$> ask
    indent     <- prIndent   <$> ask
    wrapIndent <- prWrapIndent   <$> ask
    space      <- psSpace    <$> get
    col        <- psCol      <$> get
    (b, l)     <- psQueue    <$> get
    wrapping   <- psWrapping <$> get

    let prefix
            | col == 0  = indent <> if wrapping then wrapIndent else ""
            | space     = " "
            | otherwise = ""

        dontWrapYet
            | col == 0  = True
            | otherwise = case mbMaxCol of
                Nothing     -> True
                Just maxCol -> col + T.length prefix + l <= maxCol

    if dontWrapYet
        then do
            tell $ TLB.fromText prefix <> b
            modify $ \s -> s
                { psCol   = col + T.length prefix + l
                , psSpace = False
                , psQueue = (mempty, 0)
                }

        else do
            tell "\n"
            modify $ \s -> s {psCol = 0, psSpace = False, psWrapping = True}
            unPrint printFlush


--------------------------------------------------------------------------------
printIndent :: Text -> Print a -> Print a
printIndent indent =
    Print . local (\r -> r {prIndent = prIndent r <> indent}) . unPrint


--------------------------------------------------------------------------------
printIndent4 :: Print a -> Print a
printIndent4 = printIndent "    "


--------------------------------------------------------------------------------
printWrapIndent :: Text -> Print a -> Print a
printWrapIndent indent =
    Print . local (\r -> r {prWrapIndent = indent}) . unPrint


--------------------------------------------------------------------------------
printWrapIndent8 :: Print a -> Print a
printWrapIndent8 = printWrapIndent "        "
