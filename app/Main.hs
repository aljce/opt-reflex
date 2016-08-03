module Main where

import qualified Reflex.Options as O

import Reflex.Dom hiding (switch)
import Options.Applicative

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

render :: Test -> String
render (Test name q)
  | q         = statement
  | otherwise = map toUpper statement
  where statement = "Hello, " ++ name ++ "! How are you?"


main :: IO ()
main = O.execParserReflex displayTest opts True
  where displayTest test = mainWidget $ el "div" $ text (render test)
