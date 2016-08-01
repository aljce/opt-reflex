module Reflex.Options where

import Reflex.Dom hiding (switch) --TODO: remove
import JavaScript.Web.Storage
import Data.JSString hiding (words)

import Lens.Simple ((^.))
import Data.Map

import Options.Applicative.Extra (execParserPure,ParserPrefs(..))

import Options.Applicative --TODO: remove

centered = "display: block; margin: 0px auto;"

optionsStorage = "opt-reflex-prev-command"

getSuccess :: ParserResult a -> Maybe a
getSuccess (Success a) = Just a
getSuccess _ = Nothing

inputBox :: (MonadWidget t m) => ParserInfo a -> Maybe JSString -> m (Dynamic t (ParserResult a),Event t a)
inputBox parser initialValue = do
  rec str <- textInput $ def & textInputConfig_attributes .~ attrs
                             & textInputConfig_initialValue .~ (maybe "" unpack initialValue)
      attrs <- mapDyn changeState result
      result <- mapDyn (parse parser) (str ^. textInput_value)
  pure (result, attachWithMaybe (const . getSuccess)
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

main :: (Show a) => ParserInfo a -> IO ()
main parserInfo = do
  prevCommand <- getItem optionsStorage localStorage
  mainWidget $ el "div" $ do
    (parseResult,enterPress) <- inputBox parserInfo prevCommand
    submit parseResult
    elAttr "span" ("style" =: centered) (display parseResult)

data Test = Test
  { hello :: String
  , quiet :: Bool } deriving (Show)

test :: Parser Test
test = Test
  <$> strOption
      ( long "hello"
     <> metavar "TARGET"
     <> help "Target for the greeting" )
  <*> switch
      ( long "quiet"
     <> help "Whether to be quiet" )

opts = info (helper <*> test)
      (fullDesc
      <> progDesc "Print a greeting for TARGET"
      <> header "hello - a test for optparse-applicative")
