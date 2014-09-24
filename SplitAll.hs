{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}


module Main where


import           Control.Applicative
import qualified Data.List                 as L
import           Data.Monoid
import qualified Data.Text                 as T
import           Filesystem.Path.CurrentOS hiding ((</>))
import           Prelude                   hiding (FilePath)
import           Shelly

default (T.Text)


toBoxDir :: FilePath
toBoxDir = "/Volumes/Untitled/to-box"

main :: IO ()
main = shelly $ verbosely $ chdir toBoxDir $
    mapM_ splitFile =<< L.sort . filter (hasExt "xz") <$> ls toBoxDir

splitFile :: FilePath -> Sh ()
splitFile fn = do
    mkdir_p name
    chdir name $ split (".." </> fn)
    cleanUp name
    where name = basename fn

split :: FilePath -> Sh ()
split fn = run_ "split" ["-b", "5000m", toTextIgnore fn]

cleanUp :: FilePath -> Sh ()
cleanUp fn = cleanUp' =<< ls fn
    where cleanUp' [_] =  echo ("One split. Removing " <> toTextIgnore fn)
                       >> rm_rf fn
          cleanUp' _   =  echo (toTextIgnore fn <> " split. Removing.")
                       >> rm fn