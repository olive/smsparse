module Main where

import System.Environment

import Language.Parser
import Language.Renderer
import System.IO
import Control.Applicative ((<$>))

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

main :: IO ()
main = do
    (infile:outfile:_) <- getArgs
    withFile infile ReadMode $ \h -> do
        mylines <- lines <$> hGetContents h
        let chunks = parse mylines
        let rendered = render chunks
        writeFile outfile rendered
