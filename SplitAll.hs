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

splitSize :: T.Text
splitSize = "4096m"

main :: IO ()
main = shelly $ verbosely $ chdir toBoxDir $
    mapM_ splitFile =<< L.sort . filter (hasExt "xz") <$> ls toBoxDir

splitFile :: FilePath -> Sh ()
splitFile fn = do
    mkdir_p name
    chdir name . split $ (".." :: FilePath) </> fn
    cleanUp fn name
    where name = basename fn

split :: FilePath -> Sh ()
split fn = run_ "split" ["-b", splitSize, toTextIgnore fn]

cleanUp :: FilePath -> FilePath -> Sh ()
cleanUp fn dn = cleanUp' =<< ls dn
    where cleanUp' fns
                | length fns == 1 =  echo ("One split. Removing " <> toTextIgnore dn)
                                  >> rm_rf dn
                | otherwise       =  echo (toTextIgnore fn <> " split. Removing.")
                                  >> rm fn
