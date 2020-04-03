{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Applicative (
#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,0))
#else
                            (<$>), (<*>),
#endif
#if (defined(MIN_VERSION_simple_cmd_args) && MIN_VERSION_simple_cmd_args(0,1,3))
#else
                            (<|>)
#endif
                           )
import Control.Monad (when)
import Data.Aeson (Value)
import qualified Data.ByteString.Char8 as B
import Data.Maybe
#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,11,0))
#else
import Data.Semigroup ((<>))
#endif
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Lens.Micro
import Lens.Micro.Aeson
import Network.HTTP.Simple
import SimpleCmdArgs
import System.FilePath ((</>))

import Paths_pagure_cli (version)

data Filter = Owner String (Maybe String) | All String

main :: IO ()
main =
  simpleCmdArgs (Just version) "Pagure client" "Simple pagure CLI" $
  subcommands
  [ Subcommand "list" "list projects" $
    listProjects <$> serverOpt <*> countOpt <*> jsonOpt <*> urlOpt <*> searchFilter
  , Subcommand "user" "user repos" $
    userRepos <$> serverOpt <*> countOpt <*> jsonOpt <*> urlOpt <*> strArg "USER"
--  , Subcommand "clone" "clone project" $
--    cloneProject <$> serverOpt <*> jsonOpt <*> urlOpt <*> strArg "USER"
  , Subcommand "branches" "show project branches" $
    repoBranches <$> serverOpt <*> jsonOpt <*> urlOpt <*> strArg "REPO"
  , Subcommand "issues" "list project issues" $
    projectIssues <$> serverOpt <*> countOpt <*> jsonOpt <*> urlOpt <*> strArg "REPO" <*> switchWith 'A' "all" "list Open and Closed issues" <*> optional (strOptionWith 'a' "author" "AUTHOR" "Filter issues by creator") <*> optional (strOptionWith 'S' "since" "Y-M-D" "Filter issues updated after date")
  ]
  where
    countOpt = switchWith 'c' "count" "Show number only"
    jsonOpt = switchWith 'j' "json" "Print raw json response"
    urlOpt = switchWith 'u' "url" "Show url"
    ownerOpt = strOptionWith 'o' "owner" "OWNER" "Projects with certain owner"
    serverOpt = strOptionalWith 's' "server" "SERVER" "Pagure server" "src.fedoraproject.org"
    searchFilter = All <$> strArg "PATTERN" <|>
                   Owner <$> ownerOpt <*> optional (strArg "PATTERN")

listProjects :: String -> Bool -> Bool -> Bool -> Filter -> IO ()
listProjects server count json showurl search = do
  let path = "projects"
      params = ("fork", Just "0") : owner search
  results <- queryPaged server count showurl path params ("pagination", "page")
  mapM_ (printResults json "projects" "name") results
  where
    owner :: Filter -> Query
    -- (!orphan only works on pagure >=0.29)
    owner (All s) =
      [("owner", Just "!orphan") | server == "src.fedoraproject.org"] ++
      [("pattern", Just (B.pack s))]
    owner (Owner n mpat) =
      ("owner", Just (B.pack n)) : maybeKey "pattern" mpat

userRepos :: String -> Bool -> Bool -> Bool -> String -> IO ()
userRepos server count json showurl user = do
  let path = "user" </> user
  results <- queryPaged server count showurl path [] ("repos_pagination", "repopage")
  mapM_ (printResults json "repos" "name") results

maybeKey :: String -> Maybe String -> Query
maybeKey _ Nothing = []
maybeKey k mval = [(B.pack k, fmap B.pack mval)]

printResults :: Bool -> String -> String -> Value -> IO ()
printResults json object key' result =
  if json then print result
  else
    mapM_ T.putStrLn $ result ^.. key (T.pack object) . values . key (T.pack key') . _String

projectIssues :: String -> Bool -> Bool -> Bool -> String -> Bool -> Maybe String -> Maybe String -> IO ()
projectIssues server count json showurl repo allstatus mauthor msince = do
  let path = repo </> "issues"
      params = [("status", Just "all") | allstatus] ++
               maybeKey "author" mauthor ++ maybeKey "since" msince
  results <- queryPaged server count showurl path params ("pagination", "page")
  mapM_ (printIssues "issues") results
  where
    printIssues :: String -> Value -> IO ()
    printIssues object result =
      if json then print result
      else do
        let ids = result ^.. key (T.pack object) . values . key (T.pack "id") . _Integer
            titles = result ^.. key (T.pack object) . values . key (T.pack "title") . _String
            statuses = result ^.. key (T.pack object) . values . key (T.pack "status") . _String
        mapM_ printIssue $ zip3 ids titles statuses

    printIssue :: (Integer, T.Text, T.Text) -> IO ()
    printIssue (issue, title, status) = do
      T.putStrLn $ "\"" <> title <> "\""
      putStrLn $ "https://" <> server </> repo </> "issue" </> show issue <> " (" <> T.unpack status <> ")"

queryPaged :: String -> Bool -> Bool -> String -> Query -> (String,String) -> IO [Value]
queryPaged server count showurl path params (pagination,paging) = do
  res1 <- pagureQuery showurl server path (params ++ [("per_page", Just (if count then "1" else "100"))])
  let mpages = res1 ^? key (T.pack pagination) . key "pages" . _Integer
  if count
    then do
    print $ fromMaybe (error' "pages not found") mpages
    return []
    else do
    rest <- mapM nextPage [2..(fromMaybe 0 mpages)]
    return $ res1 : rest
      where
        nextPage p =
          pagureQuery False server path (params ++ [("per_page", Just "100")] ++ maybeKey paging (Just (show p)))

pagureQuery :: Bool -> String -> String -> Query -> IO Value
pagureQuery showurl server path params = do
  let url = "https://" <> server </> "api/0" </> path
  when showurl $ putStrLn url
  req <- setRequestQueryString params <$> parseRequest url
  getResponseBody <$> httpJSON req

repoBranches :: String -> Bool -> Bool -> String -> IO ()
repoBranches server json showurl repo = do
  let path = repo </> "git/branches"
  res <- pagureQuery showurl server path []
  printKeyList json "branches" res

printKeyList :: Bool -> String -> Value -> IO ()
printKeyList json key' res =
  if json then print res
    else mapM_ T.putStrLn $ res ^.. key (T.pack key') . values . _String

-- from simple-cmd
error' :: String -> a
#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,9,0))
error' = errorWithoutStackTrace
#else
error' = error
#endif
