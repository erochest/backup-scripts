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
    files <- ls name
    if (length files == 1)
        then do
            echo $ "One split. Removing " <> toTextIgnore name
            rm_rf name
        else do
            echo $ toTextIgnore fn <> " split. Removing."
            rm fn

    where name = basename fn

split :: FilePath -> Sh ()
split fn = run_ "split" ["-b", "5000m", toTextIgnore fn]
