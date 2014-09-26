{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}


module Main where


import           Control.Applicative
import qualified Data.List                 as L
import           Data.Monoid
import qualified Data.Text                 as T
import           Filesystem
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
    exists <- liftIO $ isDirectory fn
    when exists $ do
        echo $ "clearing out " <> toTextIgnore fn
        rm_rf fn

    mkdir_p name
    chdir name $ split (".." </> fn)
    cleanUp name
    where name = basename fn

split :: FilePath -> Sh ()
split fn = run_ "split" ["-b", splitSize, toTextIgnore fn]

cleanUp :: FilePath -> Sh ()
cleanUp fn = cleanUp' =<< ls fn
    where cleanUp' fns
                | length fns == 1 =  echo ("One split. Removing " <> toTextIgnore fn)
                                  >> rm_rf fn
                | otherwise       =  echo (toTextIgnore fn <> " split. Removing.")
                                  >> rm fn
