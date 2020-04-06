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
#if (defined(MIN_VERSION_http_conduit) && MIN_VERSION_http_conduit(2,3,1))
#else
import Data.ByteString (ByteString)
#endif
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
import Network.HTTP.Conduit (queryString)
import Network.HTTP.Simple
import SimpleCmdArgs
import System.FilePath ((</>))

import Paths_pagure_cli (version)

main :: IO ()
main =
  simpleCmdArgs (Just version) "Pagure client" "Simple pagure CLI" $
  subcommands
  [ Subcommand "list" "list projects" $
    listProjects <$> serverOpt <*> countOpt <*> urlOpt <*> jsonOpt <*> forksOpt <*> optional namespaceOpt <*> optional packagerOpt <*> optional (strArg "PATTERN")
  , Subcommand "user" "user repos" $
    userRepos <$> serverOpt <*> countOpt <*> urlOpt <*> jsonOpt <*> strArg "USER"
--  , Subcommand "clone" "clone project" $
--    cloneProject <$> serverOpt <*> urlOpt <*> jsonOpt <*> strArg "USER"
  , Subcommand "branches" "list project branches" $
    repoBranches <$> serverOpt <*> urlOpt <*> jsonOpt <*> strArg "REPO"
  , Subcommand "issues" "list project issues" $
    projectIssues <$> serverOpt <*> countOpt <*> urlOpt <*> jsonOpt <*> strArg "REPO" <*> switchWith 'A' "all" "list Open and Closed issues" <*> optional (strOptionWith 'a' "author" "AUTHOR" "Filter issues by creator") <*> optional (strOptionWith 'S' "since" "Y-M-D" "Filter issues updated after date") <*> optional (strOptionWith 't' "title" "pattern" "Filter issues by title")
  , Subcommand "users" "list users" $
    users <$> serverOpt <*> urlOpt <*> jsonOpt <*> strArg "PATTERN"
  , Subcommand "groups" "list groups" $
    groups <$> serverOpt <*> countOpt <*> urlOpt <*> jsonOpt <*> optional (strArg "PATTERN")
  ]
  where
    countOpt = switchWith 'c' "count" "Show number only"
    jsonOpt = switchWith 'j' "json" "Print raw json response"
    urlOpt = switchWith 'U' "url" "Show url"
    namespaceOpt = strOptionWith 'n' "namespace" "NAMESPACE" "Specify project repo namespace"
    packagerOpt = Owner <$> ownerOpt <|> Committer <$> usernameOpt
    usernameOpt = strOptionWith 'u' "username" "USERNAME" "Projects to which username can commit"
    ownerOpt = strOptionWith 'o' "owner" "OWNER" "Projects with certain owner"
    serverOpt = strOptionalWith 's' "server" "SERVER" "Pagure server" srcFedoraprojectOrg
    forksOpt = flagWith' OnlyForks 'F' "only-forks" "Only list forks" <|>
               flagWith NoForks IncludeForks 'f' "include-forks" "Include forks [default: ignore forks]"

data Packager = Owner String | Committer String

data Forks = NoForks | IncludeForks | OnlyForks

srcFedoraprojectOrg :: String
srcFedoraprojectOrg = "src.fedoraproject.org"

#if (defined(MIN_VERSION_http_conduit) && MIN_VERSION_http_conduit(2,3,1))
#else
type Query = [(ByteString, Maybe ByteString)]
#endif

listProjects :: String -> Bool -> Bool -> Bool -> Forks -> Maybe String -> Maybe Packager -> Maybe String -> IO ()
listProjects server count showurl json forks mnamespace mpackager mpattern = do
  unless (count || isJust mpackager || isJust mpattern) $
    error' "Please give a package pattern, --count, or --owner/--username"
  let path = "projects"
      params = ("short", Just "1") : fork ++ packager ++ maybeKey "namespace" mnamespace ++ maybeKey "pattern" mpattern
  results <- queryPaged server count showurl json path params ("pagination", "page")
  unless json $
    mapM_ (printResult "projects" (if isJust mnamespace then "name" else "fullname")) results
  where
    -- (!orphan only works on pagure >=0.29)
    packager = case mpackager of
      Nothing -> [("owner", Just "!orphan") | server == srcFedoraprojectOrg]
      Just (Owner o) -> maybeKey "owner" $ Just o
      Just (Committer c) -> maybeKey "username" $ Just c

    fork = case forks of
      NoForks -> maybeKey "fork" $ Just "0"
      IncludeForks -> []
      OnlyForks -> maybeKey "fork" $ Just "1"

userRepos :: String -> Bool -> Bool -> Bool -> String -> IO ()
userRepos server count showurl json user = do
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
projectIssues server count showurl json repo allstatus mauthor msince mpat = do
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
  req <- setRequestQueryString params <$> parseRequest url
  when showurl $ putStrLn $ url ++ B.unpack (queryString req)
  if json then do
    res <- getResponseBody <$> httpLBS req
    BL.putStrLn res
    case eitherDecode res of
      Left e -> error' e
      Right v -> return v
    else getResponseBody <$> httpJSON req

repoBranches :: String -> Bool -> Bool -> String -> IO ()
repoBranches server showurl json repo = do
  let namespace =
        if server == srcFedoraprojectOrg && '/' `notElem` repo
        then "rpms/" else ""
      path = namespace ++ repo </> "git/branches"
  res <- pagureQuery showurl server json path []
  unless json $
    printKeyList "branches" res

users :: String -> Bool -> Bool -> String -> IO ()
users server showurl json pat = do
  let path = "users"
      params = maybeKey "pattern" $ Just pat
  res <- pagureQuery showurl server json path params
  unless json $
    printKeyList "users" res

groups :: String -> Bool -> Bool -> Bool -> Maybe String -> IO ()
groups server count showurl json mpat = do
  let path = "groups"
      params = maybeKey "pattern" mpat
  results <- queryPaged server count showurl json path params ("pagination", "page")
  unless json $
    mapM_ (printKeyList "groups") results

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
