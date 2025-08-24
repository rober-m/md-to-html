{-# LANGUAGE RecordWildCards #-}
module Main where

import           CLI             (Options (..), cliParser)
import           Control.Monad   (when)
import qualified Data.Text.IO    as TIO
import           MdParser        (mainParser)
import           Text.Megaparsec (parse)
import           ToHTML          (toHTML)

main :: IO ()
main = do
    Options{..} <- cliParser
    fileContent <- TIO.readFile inputFile
    when verbose $ do
        mapM_ print [ "----------------------------------------------------------"
                    , "Parsing this input file: " ++ inputFile
                    , "Writing result to this output file: " ++ outputFile
                    , "----------------------------------------------------------"
                    , "File content:"
                    , show fileContent
                    ]
    case parse mainParser inputFile fileContent of
        Left err -> print err
        Right elems -> do
            when verbose $ do
                print "----------------------------------------------------------"
                print "Parsed elements:"
                print elems
            let html = toHTML elems
            when verbose $ do
                print "----------------------------------------------------------"
                print "HTML output:"
                print html
            TIO.writeFile outputFile html
