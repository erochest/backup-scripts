{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}


module Main where


import           Data.Attoparsec.Text
import qualified Data.Text                 as T
import           Filesystem.Path.CurrentOS
import           Options.Applicative       hiding (Parser)
import qualified Options.Applicative       as A
import           Prelude                   hiding (FilePath)
import           Shelly


default (T.Text)


main :: IO ()
main = do
    RmDups{..} <- A.execParser opts
    shelly $ verbosely $
        either (liftIO . putStrLn . ("ERROR: " ++)) (chdir baseDir . mapM_ rmDup)
            =<< foldr (liftA2 (:) . parseOnly duplicate) (Right [])
            .   T.lines
            <$> readfile dupFile

-- Primary functions

rmDup :: Duplicate -> Sh ()
rmDup (primary, dupFiles) = do
    echo $  "KEEPING " <> toTextIgnore primary
         <> ": deleting " <> tshow (length dupFiles)
    mapM_ rm_f dupFiles

-- Utilities

tshow :: Show a => a -> T.Text
tshow = T.pack . show

-- Parsing

type Duplicate = (FilePath, [FilePath])

duplicate :: Parser Duplicate
duplicate = (,) <$> (filepath <* space) <*> sepBy1 filepath space

filepath :: Parser FilePath
filepath = decodeString <$> many1 (escaped <|> notChar ' ')

escaped :: Parser Char
escaped = char '\\' *> anyChar

-- Command line

data RmDups = RmDups
            { baseDir :: FilePath
            , dupFile :: FilePath
            } deriving (Show)

opts' :: A.Parser RmDups
opts' =   RmDups
      <$> fileOption (  short 'd' <> long "dir" <> metavar "DIR_NAME"
                     <> help "The base directory to process everything from.")
      <*> fileOption (  short 'i' <> long "input" <> metavar "DUP_FILE"
                     <> help "The file listing duplicates.")

opts :: A.ParserInfo RmDups
opts = info (helper <*> opts')
            (  fullDesc
            <> progDesc "Removes duplicate files from the output of fdups."
            <> header "rm-dups")

fileOption :: Mod OptionFields FilePath -> A.Parser FilePath
fileOption = A.option (pure . decodeString)
