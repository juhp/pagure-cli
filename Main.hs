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
import Control.Monad (unless, when)
import Data.Aeson (eitherDecode)
import Data.Aeson.Types
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Maybe
#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,11,0))
#else
import Data.Semigroup ((<>))
#endif
import Data.Text (Text)
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
    listProjects <$> serverOpt <*> countOpt <*> jsonOpt <*> urlOpt <*> optional namespaceOpt <*> searchFilter
  , Subcommand "user" "user repos" $
    userRepos <$> serverOpt <*> countOpt <*> jsonOpt <*> urlOpt <*> strArg "USER"
--  , Subcommand "clone" "clone project" $
--    cloneProject <$> serverOpt <*> jsonOpt <*> urlOpt <*> strArg "USER"
  , Subcommand "branches" "show project branches" $
    repoBranches <$> serverOpt <*> jsonOpt <*> urlOpt <*> strArg "REPO"
  , Subcommand "issues" "list project issues" $
    projectIssues <$> serverOpt <*> countOpt <*> jsonOpt <*> urlOpt <*> strArg "REPO" <*> switchWith 'A' "all" "list Open and Closed issues" <*> optional (strOptionWith 'a' "author" "AUTHOR" "Filter issues by creator") <*> optional (strOptionWith 'S' "since" "Y-M-D" "Filter issues updated after date") <*> optional (strOptionWith 't' "title" "pattern" "Filter issues by title")
  , Subcommand "users" "find users" $
    users <$> serverOpt <*> jsonOpt <*> urlOpt <*> strArg "PATTERN"
  ]
  where
    countOpt = switchWith 'c' "count" "Show number only"
    jsonOpt = switchWith 'j' "json" "Print raw json response"
    urlOpt = switchWith 'u' "url" "Show url"
    namespaceOpt = strOptionWith 'n' "namespace" "NAMESPACE" "Specify project repo namespace"
    ownerOpt = strOptionWith 'o' "owner" "OWNER" "Projects with certain owner"
    serverOpt = strOptionalWith 's' "server" "SERVER" "Pagure server" srcFedoraprojectOrg
    searchFilter = All <$> strArg "PATTERN" <|>
                   Owner <$> ownerOpt <*> optional (strArg "PATTERN")

srcFedoraprojectOrg :: String
srcFedoraprojectOrg = "src.fedoraproject.org"

-- FIXME show namespace?
listProjects :: String -> Bool -> Bool -> Bool -> Maybe String -> Filter -> IO ()
listProjects server count json showurl mnamespace search = do
  let path = "projects"
      params = ("fork", Just "0") : owner search ++ maybeKey "namespace" mnamespace
  results <- queryPaged server count showurl json path params ("pagination", "page")
  unless json $
    mapM_ (printResult "projects" "name") results
  where
    owner :: Filter -> Query
    -- (!orphan only works on pagure >=0.29)
    owner (All s) =
      [("owner", Just "!orphan") | server == srcFedoraprojectOrg] ++
      [("pattern", Just (B.pack s))]
    owner (Owner n mpat) =
      ("owner", Just (B.pack n)) : maybeKey "pattern" mpat

userRepos :: String -> Bool -> Bool -> Bool -> String -> IO ()
userRepos server count json showurl user = do
  let path = "user" </> user
  results <- queryPaged server count showurl json path [] ("repos_pagination", "repopage")
  unless json $
    mapM_ (printResult "repos" "name") results

maybeKey :: String -> Maybe String -> Query
maybeKey _ Nothing = []
maybeKey k mval = [(B.pack k, fmap B.pack mval)]

printResult :: String -> String -> Value -> IO ()
printResult obj key' result =
  mapM_ T.putStrLn $ result ^.. key (T.pack obj) . values . key (T.pack key') . _String

-- FIXME limit max number of issues
projectIssues :: String -> Bool -> Bool -> Bool -> String -> Bool -> Maybe String -> Maybe String -> Maybe String -> IO ()
projectIssues server count json showurl repo allstatus mauthor msince mpat = do
  let path = repo </> "issues"
      params = [("status", Just "all") | allstatus] ++
               maybeKey "author" mauthor ++ maybeKey "since" msince
  results <- queryPaged server count showurl json path params ("pagination", "page")
  unless json $
    mapM_ printIssues results
  where
    printIssues :: Value -> IO ()
    printIssues result = do
      let issues = result ^.. key (T.pack "issues") . values . _Object
      mapM_ printIssue issues

    parseIssue :: Object -> Maybe (Integer, Text, Text)
    parseIssue =
      parseMaybe $ \obj -> do
        id' <- obj .: "id"
        title <- obj .: "title"
        status <- obj .: "status"
        return (id',title,status)

    printIssue :: Object -> IO ()
    printIssue issue = do
      let mfields = parseIssue issue
      case mfields of
        Nothing -> putStrLn "parsing issue failed"
        Just (id',title,status) ->
          when (isNothing mpat || T.pack (fromJust mpat) `T.isInfixOf` title) $
          putStrLn $ "https://" <> server </> repo </> "issue" </> show id' <> " (" <> T.unpack status <> "): " <> T.unpack title

-- FIXME limit max number of pages (10?) or --pages
queryPaged :: String -> Bool -> Bool -> Bool -> String -> Query -> (String,String) -> IO [Value]
queryPaged server count showurl json path params (pagination,paging) = do
  res1 <- pagureQuery showurl server json path (params ++ [("per_page", Just (if count then "1" else "100"))])
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
          pagureQuery False server json path (params ++ [("per_page", Just "100")] ++ maybeKey paging (Just (show p)))

pagureQuery :: Bool -> String -> Bool -> String -> Query -> IO Value
pagureQuery showurl server json path params = do
  let url = "https://" <> server </> "api/0" </> path
  when showurl $ putStrLn url
  req <- setRequestQueryString params <$> parseRequest url
  if json then do
    res <- getResponseBody <$> httpLBS req
    BL.putStrLn res
    case eitherDecode res of
      Left e -> error' e
      Right v -> return v
    else getResponseBody <$> httpJSON req

repoBranches :: String -> Bool -> Bool -> String -> IO ()
repoBranches server json showurl repo = do
  let namespace =
        if server == srcFedoraprojectOrg && '/' `notElem` repo
        then "rpms/" else ""
      path = namespace ++ repo </> "git/branches"
  res <- pagureQuery showurl server json path []
  unless json $
    printKeyList "branches" res

users :: String -> Bool -> Bool -> String -> IO ()
users server json showurl pat = do
  let path = "users"
      params = maybeKey "pattern" $ Just pat
  res <- pagureQuery showurl server json path params
  unless json $
    printKeyList "users" res

printKeyList :: String -> Value -> IO ()
printKeyList key' res =
  mapM_ T.putStrLn $ res ^.. key (T.pack key') . values . _String

-- from simple-cmd
error' :: String -> a
#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,9,0))
error' = errorWithoutStackTrace
#else
error' = error
#endif
