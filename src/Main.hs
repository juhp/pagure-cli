{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Applicative (
#if !MIN_VERSION_base(4,8,0)
                            (<$>), (<*>),
#endif
#if !MIN_VERSION_simple_cmd_args(0,1,3)
                            (<|>)
#endif
                           )
import Control.Monad (unless, when)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Types
#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.KeyMap as M
#else
import qualified Data.HashMap.Lazy as M
#endif
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Yaml (encode)
import Network.HTTP.Query ((+/+))
import SimpleCmd (error', (+-+))
import SimpleCmdArgs
import Fedora.Pagure

import Paths_pagure_cli (version)

main :: IO ()
main =
  simpleCmdArgs (Just version) "Pagure client" "Simple pagure CLI" $
  subcommands
  [ Subcommand "list" "list projects" $
    listProjects
    <$> serverOpt
    <*> countOpt
    <*> formatOpt
    <*> forksOpt
    <*> optional namespaceOpt
    <*> optional packagerOpt
    <*> optional (strArg "PATTERN")
  , Subcommand "user" "list user repos" $
    userRepos
    <$> serverOpt
    <*> countOpt
    <*> switchWith 'f' "forks" "List user's forks"
    <*> strArg "USER"
  , Subcommand "group" "list group repos" $
    groupProjects
    <$> serverOpt
    <*> switchWith 'c' "count" "Count projects"
    <*> strArg "GROUP"
  , Subcommand "project" "show project details" $
    projectInfo
    <$> serverOpt
    <*> formatOpt
    <*> strArg "PROJECT"
  , Subcommand "branches" "list project branches" $
    repoBranches
    <$> serverOpt
    <*> formatOpt
    <*> strArg "REPO"
  , Subcommand "git-url" "show project repo's git urls" $
    gitUrl
    <$> serverOpt
    <*> formatOpt
    <*> strArg "REPO"
  , Subcommand "issues" "list project issues" $
    projectIssues
    <$> serverOpt
    <*> countOpt
    <*> formatOpt
    <*> strArg "REPO"
    <*> switchWith 'A' "all" "list Open and Closed issues"
    <*> optional (strOptionWith 'a' "author" "AUTHOR" "Filter issues by creator")
    <*> optional (strOptionWith 'S' "since" "Y-M-D" "Filter issues updated after date")
    <*> optional (strOptionWith 't' "title" "pattern" "Filter issues by title")
  , Subcommand "issue" "show project issue" $
    projectIssue
    <$> serverOpt
    <*> formatOpt
    <*> strArg "REPO"
    <*> argumentWith auto "ISSUE"
  , Subcommand "users" "list users" $
    users
    <$> serverOpt
    <*> formatOpt
    <*> strArg "PATTERN"
  , Subcommand "username" "fullname of user" $
    username
    <$> serverOpt
    <*> formatOpt
    <*> strArg "USERNAME"
  , Subcommand "userinfo" "show user details" $
    userInfo
    <$> serverOpt
    <*> formatOpt
    <*> strArg "USERNAME"
  , Subcommand "groups" "list groups" $
    groups
    <$> serverOpt
    <*> countOpt
    <*> formatOpt
    <*> optional (strArg "PATTERN")
  , Subcommand "groupinfo" "show group details" $
    groupInfo
    <$> serverOpt
    <*> switchWith 'p' "projects" "List projects"
    <*> formatOpt
    <*> strArg "GROUP"
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

defaultPrinter :: OutputFormat -> (Object -> IO ()) -> Object -> IO ()
defaultPrinter FormatDefault pr = pr
defaultPrinter FormatJson _ = BL.putStrLn . encodePretty
defaultPrinter FormatYaml _ = B.putStrLn . encode

listProjects :: String -> Bool -> OutputFormat -> Forks -> Maybe String -> Maybe Packager -> Maybe String -> IO ()
listProjects server count format forks mnamespace mpackager mpattern = do
  unless (count || isJust mpackager || isJust mpattern) $
    error' "Please give a package pattern, --count, or --owner/--username"
  let path = "projects"
      params = makeKey "short" "1" ++ fork ++ packager ++ maybeKey "namespace" mnamespace ++ maybeKey "pattern" mpattern
  pages <- queryPagureCountPaged server count path params ("pagination", "page")
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

    printPage :: Object -> IO ()
    printPage result = do
      let key' = if isJust mnamespace then "name" else "fullname"
          projects = lookupKey' "projects" result
      (mapM_ T.putStrLn . mapMaybe (lookupKey key')) projects

-- FIXME duplicates subset of listProjects
userRepos :: String -> Bool -> Bool -> String -> IO ()
userRepos server count forks user =
  if count then do
    let path = "user" +/+ user
    mcnt <- queryPagureCount server path [] $
            if forks then "forks_pagination" else "repos_pagination"
    print $
      fromMaybe
      (error' ("number of" +-+ (if forks then "forks" else "repos") +-+ "could not be determined"))
      mcnt
    else do
    repos <- (if forks then pagureUserForks else pagureUserRepos) server user
    mapM_ T.putStrLn repos

boolKey :: String -> Bool -> String -> Query
boolKey _ False _ = []
boolKey k True val = makeKey k val

-- FIXME limit max number of issues
projectIssues :: String -> Bool -> OutputFormat -> String -> Bool -> Maybe String -> Maybe String -> Maybe String -> IO ()
projectIssues server count format repo allstatus mauthor msince mpat = do
  let path = repo +/+ "issues"
      params = [makeItem "status" "all" | allstatus] ++
               maybeKey "author" mauthor ++ maybeKey "since" msince
  pages <- queryPagureCountPaged server count path params ("pagination", "page")
  mapM_ (defaultPrinter format printIssues) pages
  where
    printIssues :: Object -> IO ()
    printIssues result = do
      let issues = lookupKey' "issues" result :: [Object]
      mapM_ printIssue issues

    printIssue :: Object -> IO ()
    printIssue issue = do
      let mfields = parseIssue issue
      case mfields of
        Nothing -> putStrLn "parsing issue failed"
        Just (id',title,status) ->
          when (isNothing mpat || T.pack (fromJust mpat) `T.isInfixOf` title) $
          putStrLn $ "https://" ++ server +/+ repo +/+ "issue" +/+ show id' +-+ "(" ++ T.unpack status ++ "):" +-+ T.unpack title

    parseIssue :: Object -> Maybe (Integer, Text, Text)
    parseIssue =
      parseMaybe $ \obj -> do
        id' <- obj .: "id"
        title <- obj .: "title"
        status <- obj .: "status"
        return (id',title,status)

repoBranches :: String -> OutputFormat -> String -> IO ()
repoBranches server format repo = do
  let namespace =
        if server == srcFedoraprojectOrg && '/' `notElem` repo
        then "rpms/" else ""
      path = namespace ++ repo +/+ "git/branches"
  eres <- queryPagureSingle server path []
  either error' (defaultPrinter format (printKeyList "branches")) eres

users :: String -> OutputFormat -> String -> IO ()
users server format pat = do
  let path = "users"
      params = makeKey "pattern" pat
  res <- queryPagure server path params
  defaultPrinter format (printKeyList "users") res

username :: String -> OutputFormat -> String -> IO ()
username server format user = do
  let path = "user" +/+ user
  eres <- queryPagureSingle server path $ makeKey "per_page" "1"
  case eres of
    Left err -> error' err
    Right res -> defaultPrinter format printName res
  where
    printName res =
      case lookupKey "user" res >>= lookupKey "fullname" of
        Nothing -> error' "User fullname not found"
        Just fn -> T.putStrLn fn

groups :: String -> Bool -> OutputFormat -> Maybe String -> IO ()
groups server count format mpat = do
  let path = "groups"
      params = maybeKey "pattern" mpat
  pages <- queryPagureCountPaged server count path params ("pagination", "page")
  mapM_ (defaultPrinter format (printKeyList "groups")) pages

printKeyList :: String -> Object -> IO ()
printKeyList key' res =
  mapM_ T.putStrLn (lookupKey' (T.pack key') res :: [Text])

gitUrl :: String -> OutputFormat -> String -> IO ()
gitUrl server format repo = do
  let namespace =
        if server == srcFedoraprojectOrg && '/' `notElem` repo
        then "rpms/" else ""
      path = namespace ++ repo +/+ "git/urls"
  res <- queryPagure server path []
  defaultPrinter format printURLs res
  where
    printURLs result =
      mapM_ T.putStrLn $
      M.elems $ localLookupKey "urls" result

    localLookupKey :: String -> Object ->
#if MIN_VERSION_aeson(2,0,0)
                      M.KeyMap Text
#else
                      M.HashMap Text Text
#endif
    localLookupKey = lookupKey' . T.pack

yamlPrinter :: OutputFormat -> Object -> IO ()
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
  either error' (yamlPrinter format) eval

userInfo :: String -> OutputFormat -> String -> IO ()
userInfo server format user = do
  eval <- pagureUserInfo server user []
  either error' (yamlPrinter format) eval

groupInfo :: String -> Bool -> OutputFormat -> String -> IO ()
groupInfo server projects format group = do
  let params = [makeItem "projects" "1" | projects]
  eval <- pagureGroupInfo server group params
  either error' (yamlPrinter format) eval

-- FIXME support acl parameter (admin, commit or ticket)
groupProjects :: String -> Bool -> String -> IO ()
groupProjects server count group = do
  let path = "group" +/+ group
      params = makeKey "projects" "1"
  pages <- queryPagureCountPaged server count path params ("pagination", "page")
  mapM_ (defaultPrinter FormatDefault printPage) pages
  where
    -- acl
    -- packager = case mpackager of
    --   Nothing -> boolKey "owner" (server == srcFedoraprojectOrg) "!orphan"
    --   Just (Owner o) -> makeKey "owner" o
    --   Just (Committer c) -> makeKey "username" c

    printPage :: Object -> IO ()
    printPage result = do
     let projects = lookupKey' "projects" result
     (mapM_ T.putStrLn . mapMaybe (lookupKey "fullname")) projects
