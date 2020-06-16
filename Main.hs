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
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Types
#if (defined(VERSION_lens_aeson))
import Control.Lens
import Data.Aeson.Lens
#else
import Lens.Micro
import Lens.Micro.Aeson
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
import Data.Yaml (encode)
import SimpleCmdArgs
import System.FilePath ((</>))
import Fedora.Pagure

import Paths_pagure_cli (version)

main :: IO ()
main =
  simpleCmdArgs (Just version) "Pagure client" "Simple pagure CLI" $
  subcommands
  [ Subcommand "list" "list projects" $
    listProjects <$> serverOpt <*> countOpt <*> formatOpt <*> forksOpt <*> optional namespaceOpt <*> optional packagerOpt <*> optional (strArg "PATTERN")
  , Subcommand "user" "user repos" $
    userRepos <$> serverOpt <*> countOpt <*> switchWith 'f' "forks" "List user's forks" <*> strArg "USER"
  , Subcommand "branches" "list project branches" $
    repoBranches <$> serverOpt <*> formatOpt <*> strArg "REPO"
  , Subcommand "issues" "list project issues" $
    projectIssues <$> serverOpt <*> countOpt <*> formatOpt <*> strArg "REPO" <*> switchWith 'A' "all" "list Open and Closed issues" <*> optional (strOptionWith 'a' "author" "AUTHOR" "Filter issues by creator") <*> optional (strOptionWith 'S' "since" "Y-M-D" "Filter issues updated after date") <*> optional (strOptionWith 't' "title" "pattern" "Filter issues by title")
  , Subcommand "users" "list users" $
    users <$> serverOpt <*> formatOpt <*> strArg "PATTERN"
  , Subcommand "username" "fullname of user" $
    username <$> serverOpt <*> formatOpt <*> strArg "USERNAME"
  , Subcommand "groups" "list groups" $
    groups <$> serverOpt <*> countOpt <*> formatOpt <*> optional (strArg "PATTERN")
  , Subcommand "git-url" "show project repo's git urls" $
    gitUrl <$> serverOpt <*> formatOpt <*> strArg "REPO"
  , Subcommand "project" "show project details" $
    projectInfo <$> serverOpt <*> formatOpt <*> strArg "PROJECT"
  , Subcommand "issue" "show project issue" $
    projectIssue <$> serverOpt <*> formatOpt <*> strArg "REPO" <*> argumentWith auto "ISSUE"
  , Subcommand "userinfo" "show user details" $
    userInfo <$> serverOpt <*> formatOpt <*> strArg "USERNAME"
  ]
  where
    countOpt = switchWith 'c' "count" "Show number only"
    formatOpt = flagWith' FormatJson 'j' "json" "Output JSON" <|> flagWith FormatDefault FormatYaml 'y' "yaml" "YAML output"
    namespaceOpt = strOptionWith 'n' "namespace" "NAMESPACE" "Specify project repo namespace"
    packagerOpt = Owner <$> ownerOpt <|> Committer <$> usernameOpt
    usernameOpt = strOptionWith 'u' "username" "USERNAME" "Projects to which username can commit"
    ownerOpt = strOptionWith 'o' "owner" "OWNER" "Projects with certain owner"
    serverOpt = strOptionalWith 's' "server" "SERVER" "Pagure server" srcFedoraprojectOrg
    forksOpt = flagWith' OnlyForks 'F' "only-forks" "Only list forks" <|>
               flagWith NoForks IncludeForks 'f' "include-forks" "Include forks [default: ignore forks]"

data OutputFormat = FormatDefault | FormatJson | FormatYaml

data Packager = Owner String | Committer String

data Forks = NoForks | IncludeForks | OnlyForks

srcFedoraprojectOrg :: String
srcFedoraprojectOrg = "src.fedoraproject.org"

defaultPrinter :: OutputFormat -> (Value -> IO ()) -> Value -> IO ()
defaultPrinter FormatDefault pr = pr
defaultPrinter FormatJson _ = BL.putStrLn . encodePretty
defaultPrinter FormatYaml _ = B.putStrLn . encode

listProjects :: String -> Bool -> OutputFormat -> Forks -> Maybe String -> Maybe Packager -> Maybe String -> IO ()
listProjects server count format forks mnamespace mpackager mpattern = do
  unless (count || isJust mpackager || isJust mpattern) $
    error' "Please give a package pattern, --count, or --owner/--username"
  let path = "projects"
      params = makeKey "short" "1" ++ fork ++ packager ++ maybeKey "namespace" mnamespace ++ maybeKey "pattern" mpattern
  pages <- queryPaged server count path params ("pagination", "page")
  mapM_ (defaultPrinter format printPage) pages
  where
    -- (!orphan only works on pagure >=0.29)
    packager = case mpackager of
      Nothing -> boolKey "owner" (server == srcFedoraprojectOrg) "!orphan"
      Just (Owner o) -> makeKey "owner" o
      Just (Committer c) -> makeKey "username" c

    fork = case forks of
      NoForks -> makeKey "fork" "0"
      IncludeForks -> []
      OnlyForks -> makeKey "fork" "1"

    printPage :: Value -> IO ()
    printPage result =
      let key' = if isJust mnamespace then "name" else "fullname" in
      mapM_ T.putStrLn $ result ^.. key "projects" . values . key (T.pack key') . _String

userRepos :: String -> Bool -> Bool -> String -> IO ()
userRepos server count forks user =
  if forks then do
    if count then do
      let path = "user" </> user
      mcnt <- queryPagureCount server path [] "forks_pagination"
      print $ fromMaybe (error' "number of forks could not be determined") mcnt
      else do
      repos <- pagureUserForks server user
      mapM_ T.putStrLn repos
    else do
    if count then do
      let path = "user" </> user
      mcnt <- queryPagureCount server path [] "repos_pagination"
      print $ fromMaybe (error' "number of repos could not be determined") mcnt
      else do
      repos <- pagureUserRepos server user
      mapM_ T.putStrLn repos

-- maybeKey :: String -> Maybe String -> Query
-- maybeKey _ Nothing = []
-- maybeKey k mval = [(B.pack k, fmap B.pack mval)]

-- makeKey :: String -> String -> Query
-- makeKey k val = [(B.pack k, Just (B.pack val))]

boolKey :: String -> Bool -> String -> Query
boolKey _ False _ = []
boolKey k True val = makeKey k val

-- FIXME limit max number of issues
projectIssues :: String -> Bool -> OutputFormat -> String -> Bool -> Maybe String -> Maybe String -> Maybe String -> IO ()
projectIssues server count format repo allstatus mauthor msince mpat = do
  let path = repo </> "issues"
      params = [("status", Just "all") | allstatus] ++
               maybeKey "author" mauthor ++ maybeKey "since" msince
  pages <- queryPaged server count path params ("pagination", "page")
  mapM_ (defaultPrinter format printIssues) pages
  where
    printIssues :: Value -> IO ()
    printIssues result = do
      let issues = result ^.. key (T.pack "issues") . values . _Object
      mapM_ printIssue issues

    printIssue :: Object -> IO ()
    printIssue issue = do
      let mfields = parseIssue issue
      case mfields of
        Nothing -> putStrLn "parsing issue failed"
        Just (id',title,status) ->
          when (isNothing mpat || T.pack (fromJust mpat) `T.isInfixOf` title) $
          putStrLn $ "https://" <> server </> repo </> "issue" </> show id' <> " (" <> T.unpack status <> "): " <> T.unpack title

    parseIssue :: Object -> Maybe (Integer, Text, Text)
    parseIssue =
      parseMaybe $ \obj -> do
        id' <- obj .: "id"
        title <- obj .: "title"
        status <- obj .: "status"
        return (id',title,status)

-- FIXME limit max number of pages (10?) or --pages
queryPaged :: String -> Bool -> String -> Query -> (String,String) -> IO [Value]
queryPaged server count path params (pagination,paging) =
  if count
    then do
    mnum <- queryPagureCount server path params pagination
    print $ fromMaybe (error' "pages not found") mnum
    return []
    else
    queryPagurePaged server path params (pagination,paging)

repoBranches :: String -> OutputFormat -> String -> IO ()
repoBranches server format repo = do
  let namespace =
        if server == srcFedoraprojectOrg && '/' `notElem` repo
        then "rpms/" else ""
      path = namespace ++ repo </> "git/branches"
  res <- queryPagure server path []
  defaultPrinter format (printKeyList "branches") res

users :: String -> OutputFormat -> String -> IO ()
users server format pat = do
  let path = "users"
      params = makeKey "pattern" pat
  res <- queryPagure server path params
  defaultPrinter format (printKeyList "users") res

username :: String -> OutputFormat -> String -> IO ()
username server format user = do
  let path = "user" </> user
  res <- queryPagure server path $ makeKey "per_page" "1"
  defaultPrinter format printName res
  where
    printName res =
      case res ^? key "user" . key "fullname" . _String of
        Nothing -> error' "User fullname not found"
        Just fn -> T.putStrLn fn

groups :: String -> Bool -> OutputFormat -> Maybe String -> IO ()
groups server count format mpat = do
  let path = "groups"
      params = maybeKey "pattern" mpat
  pages <- queryPaged server count path params ("pagination", "page")
  mapM_ (defaultPrinter format (printKeyList "groups")) pages

printKeyList :: String -> Value -> IO ()
printKeyList key' res =
  mapM_ T.putStrLn $ res ^.. key (T.pack key') . values . _String

gitUrl :: String -> OutputFormat -> String -> IO ()
gitUrl server format repo = do
  let namespace =
        if server == srcFedoraprojectOrg && '/' `notElem` repo
        then "rpms/" else ""
      path = namespace ++ repo </> "git/urls"
  res <- queryPagure server path []
  defaultPrinter format printURLs res
  where
    printURLs :: Value -> IO ()
    printURLs result =
      mapM_ T.putStrLn $ result ^.. key (T.pack "urls") . members . _String

-- from simple-cmd
error' :: String -> a
#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,9,0))
error' = errorWithoutStackTrace
#else
error' = error
#endif

yamlPrinter :: OutputFormat -> Value -> IO ()
yamlPrinter FormatDefault = yamlPrinter FormatYaml
yamlPrinter FormatJson = BL.putStrLn . encodePretty
yamlPrinter FormatYaml = B.putStrLn . encode

projectInfo :: String -> OutputFormat -> String -> IO ()
projectInfo server format repo = do
  let namespace =
        if server == srcFedoraprojectOrg && '/' `notElem` repo
        then "rpms/" else ""
      path = namespace ++ repo
  eval <- pagureProjectInfo server path
  either error' (yamlPrinter format) eval

projectIssue :: String -> OutputFormat -> String -> Int -> IO ()
projectIssue server format repo issue = do
  eval <- pagureProjectIssueInfo server repo issue
  either error' (yamlPrinter format . Object) eval

userInfo :: String -> OutputFormat -> String -> IO ()
userInfo server format user = do
  eval <- pagureUserInfo server user []
  either error' (yamlPrinter format) eval
