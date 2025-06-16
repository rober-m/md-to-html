module CLI where

import Options.Applicative


data Options = Options
  { inputFile :: String
  , outputFile :: String
  , verbose :: Bool
  } deriving (Show)

options :: Parser Options
options = Options
  <$> strOption
      ( long "input"
     <> short 'i'
     <> metavar "INPUT_FILE"
     <> help "Input file to process" )
  <*> strOption
      ( long "output"
     <> short 'o'
     <> metavar "OUTPUT_FILE"
     <> help "Output file to write results" )
  <*> switch
      ( long "verbose"
     <> short 'v'
     <> help "Enable verbose output" )


cliParser :: ParserInfo Options
cliParser = info (options <**> helper)
  ( fullDesc
 <> progDesc "Provide a Markdown input file to obtain an HTML output file"
 <> header "CLI Tool - A simple command line to convert Markdown to HTML" )
