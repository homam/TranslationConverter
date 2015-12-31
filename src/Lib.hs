{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, RankNTypes, DeriveGeneric #-}
module Lib(
  execute
)
where

import GHC.Generics
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as AP
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.Vector as V
import Data.Csv
import qualified Data.ByteString.Lazy as BL
import System.Directory
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Identity
import qualified Control.Monad.Reader as R
import Data.List (isSuffixOf)
import Data.Either (partitionEithers)

---

type App m = ReaderT String m
runApp :: App m a -> String -> m a
runApp = runReaderT


type Dic = M.Map String String


data Content = Content {
    language :: String
  , texts :: Dic
} deriving (Generic, Show)

instance A.ToJSON Content


--
processAllCSVFiles :: App IO ()
processAllCSVFiles = do
  dir <- ask
  files <- lift $ filter (isSuffixOf ".csv") <$> getDirectoryContents dir
  jsons <- mapM (readCSVFile . takeWhile (/='.')) files
  lift $ sh dir $ gatherResultsOrError jsons
  where
    sh _ (Left err) = print err
    sh dir (Right vs) = do
      let json = AP.encodePretty vs
      BL.writeFile (dir ++ "output/all.json") json
      C8.putStrLn json

readCSVFile :: String -> App IO (Either String Content)
readCSVFile lang = do
  dir <- ask
  csvData <- lift $ BL.readFile $ dir ++ lang ++ ".csv"
  return $ toContent lang csvData



toContent :: String -> BL.ByteString -> Either String Content
toContent lang csvData = do
  vs <- decode NoHeader csvData
  let dic = M.fromList $ map toHash $ dropWhile ((== "id") . head) $ V.toList vs
  return Content {
      language  = lang
    , texts = dic
  }

toHash :: [String] -> (String, String)
toHash ss = (head ss, last . filter (/= "") $ ss)


--- either helper functions

mapBoth :: (a -> c) -> (b -> d) -> Either a b -> Either c d
mapBoth f _ (Left x)  = Left (f x)
mapBoth _ f (Right x) = Right (f x)

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f = mapBoth f id


gatherResultsOrErrors :: [Either e a] -> Either [e] [a]
gatherResultsOrErrors eithers =
    case partitionEithers eithers of
        ([],results) -> Right results
        (errors,_) -> Left errors

gatherResultsOrError :: Monoid e => [Either e a] -> Either e [a]
gatherResultsOrError = mapLeft mconcat . gatherResultsOrErrors

---

execute :: FilePath -> IO ()
execute = runApp processAllCSVFiles
