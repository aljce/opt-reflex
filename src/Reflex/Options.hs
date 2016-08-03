module Reflex.Options (execParserReflex) where

import Reflex.Dom
import JavaScript.Web.Storage
import Data.JSString hiding (words)

import Lens.Simple ((^.))
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.STM (atomically, putTMVar, TMVar)
import Control.Exception      (throwIO,Exception(..))
import Options.Applicative.Extra (execParserPure)
import Options.Applicative.Types (ParserPrefs(..),ParserInfo(..),ParserResult(..))

import GHCJS.Internal.Options (mainWith)

centered = "display: block; margin: 0px auto;"

optionsStorage = "opt-reflex-prev-command"

getSuccess :: ParserResult a -> Maybe a
getSuccess (Success a) = Just a
getSuccess _ = Nothing

inputBox :: (MonadWidget t m) => ParserInfo a ->
                                 Maybe JSString ->
                                 m (Dynamic t (ParserResult a), Dynamic t String, Event t a)
inputBox parser initialValue = do
  rec str <- textInput $ def & textInputConfig_attributes .~ attrs
                             & textInputConfig_initialValue .~ (maybe "" unpack initialValue)
      attrs <- mapDyn changeState result
      result <- mapDyn (parse parser) (str ^. textInput_value)
  pure (result, str ^. textInput_value,
         attachWithMaybe (const . getSuccess)
                         (current result)
                         (textInputGetEnter str))
  where parseError = "style" =: ("border-color: red; " ++ centered)
        noError = "style" =: ("border-color: green; " ++ centered)
        changeState (Failure _) = parseError
        changeState _ = noError
        parse parserInfo = execParserPure parserPrefs parserInfo . words
        parserPrefs = ParserPrefs "" False False True 80

submit :: (MonadWidget t m) => Dynamic t (ParserResult a) -> m (Event t a)
submit parseResult = elAttr "span" ("style" =: centered) $ do
  parseSuccess <- mapDyn isSuccess parseResult
  click <- button "Sumbit"
  pure (attachWithMaybe (const . getSuccess) (current parseResult) click)
  where isSuccess (Success _) = True
        isSuccess _ = False

newtype Exit = Exit String

instance Show Exit where
  show (Exit str) = str

instance Exception Exit where

parse :: (Show a) => ParserInfo a -> TMVar a -> IO ()
parse parserInfo send = do
  prevCommand <- getItem optionsStorage localStorage
  mainWidget $ el "div" $ do
    (parseResult, textInput, enterPress) <- inputBox parserInfo prevCommand
    clickPress <- submit parseResult
    elAttr "span" ("style" =: centered) (display parseResult)
    unitDyn <- foldDynM sendAndKill () (attach (current textInput) (leftmost [enterPress, clickPress]))
    return (unitDyn `seq` ())
  where sendAndKill (str,a) () = liftIO $ do
          setItem optionsStorage (pack str) localStorage
          atomically (putTMVar send a)
          (throwIO . Exit) "sending program options"

execParserReflex :: (Show a) => (Maybe a -> IO b) -> ParserInfo a -> Bool -> IO b
execParserReflex main parser = mainWith main (parse parser)
