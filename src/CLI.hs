module CLI where

import           Options.Applicative

data Options = Options
  { inputFile  :: String
  , outputFile :: String
  , verbose    :: Bool
  } deriving (Show)


cliParser :: IO Options
cliParser = execParser $ info (options <**> helper) infoModifiers

options :: Parser Options
options = Options <$>
    strOption
      (  short 'i'
      <> long "input"
      <> help "Input file to process"
      <> metavar "INPUT_FILE"
      )
    <*> strOption
      (  short 'o'
      <> long "output"
      <> help "Output file to write results"
      <> metavar "OUTPUT_FILE"
      )
    <*> switch
      (  short 'v'
      <> long "verbose"
      <> help "Enable verbose output"
      )

infoModifiers :: InfoMod Options
infoModifiers = fullDesc
  <> progDesc "Process an input file and write results to an output file"
  <> header "CLI Tool - A simple command line interface for file processing"

