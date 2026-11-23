{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Fedora.Pagure.Client (
  OutputFormat(..),
  Packager(..),
  Forks(..),
  srcFedoraprojectOrg,
  defaultPrinter,
  yamlPrinter,
  listProjects,
  projectIssues,
  projectPRs,
  projectInfo,
  projectIssue,
  projectPR,
  repoBranches,
  users,
  userRepos,
  userInfo,
  username,
  userIssues,
  userRequestsFiled,
  userRequestsActionable,
  userActivityStats,
  userActivityDate,
  groups,
  groupInfo,
  groupProjects,
  gitUrl,
  contributors,
  )
where

import Control.Applicative (
#if !MIN_VERSION_base(4,8,0)
                            (<$>), (<*>),
#endif
                           )
import Control.Monad.Extra (unless, when, whenJust)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Types
#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.KeyMap as M
#else
import qualified Data.HashMap.Lazy as M
#endif
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List.Extra (lower)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import Data.Yaml (encode)
import Network.HTTP.Query ((+/+))
import SimpleCmd (error', (+-+))
import Fedora.Pagure

data OutputFormat = FormatDefault
                  | FormatJson
                  | FormatYaml

data Packager = Owner String
              | Committer String

data Forks = NoForks
           | IncludeForks
           | OnlyForks

-- | Fedora dist-git
srcFedoraprojectOrg :: String
srcFedoraprojectOrg = "src.fedoraproject.org"

-- | default print helper
defaultPrinter :: Bool -- ^ no hide empty
               -> OutputFormat
               -> (Object -> IO ()) -- ^ printer
               -> Object -- ^ JSON
               -> IO ()
defaultPrinter _ FormatDefault pr = pr
defaultPrinter nohide fmt _ = yamlPrinter nohide fmt

-- | List projects
listProjects :: String -- ^ server
             -> Bool -- ^ count
             -> Bool -- ^ no hide
             -> OutputFormat
             -> Forks
             -> Maybe String -- ^ namespace
             -> Maybe Packager
             -> Maybe String -- ^ pattern
             -> IO ()
listProjects server count nohide format forks mnamespace mpackager mpattern = do
  unless (count || isJust mpackager || isJust mpattern) $
    error' "Please give a package pattern, --count, or --owner/--username"
  let path = "projects"
      params = makeKey "short" "1" ++ fork ++ packager ++ maybeKey "namespace" mnamespace ++ maybeKey "pattern" mpattern
  pages <- queryPagureCountPaged server count path params ("pagination", "page")
  let projects = concatMap (lookupKey' "projects") pages
  mapM_ (defaultPrinter nohide format printProject) projects
  where
    -- (!orphan only works on pagure >=0.29)
    packager = case mpackager of
      Nothing -> boolKey (server == srcFedoraprojectOrg) "owner" "!orphan"
      Just (Owner o) -> makeKey "owner" o
      Just (Committer c) -> makeKey "username" c

    fork = case forks of
      NoForks -> makeKey "fork" "0"
      IncludeForks -> []
      OnlyForks -> makeKey "fork" "1"

    printProject :: Object -> IO ()
    printProject project = do
      let key' = if isJust mnamespace then "name" else "fullname"
      whenJust (lookupKey key' project) T.putStrLn

-- FIXME duplicates subset of listProjects
-- | Variant of `pagureUserRepos` or `pagureUserForks`
userRepos :: String -- ^ server
          -> Bool -- ^ count
          -> Bool -- ^ forks
          -> String -- ^ user
          -> IO ()
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

boolKey :: Bool -> String -> String -> Query
boolKey False _ _ = []
boolKey True k val = makeKey k val

-- FIXME limit max number of issues
-- | List project issues
--
-- Please use allstatus with care since it can list of a lot issues
projectIssues :: String -- ^ server
              -> Bool -- ^ count
              -> Bool -- ^ no hide
              -> OutputFormat
              -> Bool -- ^ all status
              -> Maybe String -- ^ author
              -> Maybe String -- ^ since
              -> Maybe String -- ^ pattern
              -> String -- ^ repo
              -> IO ()
projectIssues server count nohide format allstatus mauthor msince mpat repo = do
  let path = repo +/+ "issues"
      params = boolKey allstatus "status" "all" ++
               maybeKey "author" mauthor ++ maybeKey "since" msince
  pages <- queryPagureCountPaged server count path params ("pagination", "page")
  mapM_ (defaultPrinter nohide format printIssues) pages
  where
    printIssues :: Object -> IO ()
    printIssues result = do
      let issues = lookupKey' "issues" result :: [Object]
      mapM_ printIssue issues

    printIssue :: Object -> IO ()
    printIssue issue = do
      case parseIssue issue of
        Nothing -> putStrLn "parsing issue failed"
        Just (id',title,status) ->
          when (isNothing mpat || T.pack (fromJust mpat) `T.isInfixOf` title) $
          putStrLn $ "https://" ++ server +/+ repo +/+ "issue" +/+ show id' ++ (if allstatus then " (" ++ T.unpack status ++ ")" else "") +-+ ":" +-+ T.unpack title

    parseIssue :: Object -> Maybe (Integer, Text, Text)
    parseIssue =
      parseMaybe $ \obj -> do
        id' <- obj .: "id"
        title <- obj .: "title"
        status <- obj .: "status"
        return (id',title,status)

-- FIXME limit max number of issues
-- | List pull-requests
--
-- Please use allstatus with care since it can list of a lot of requests
projectPRs :: String -- ^ server
           -> Bool -- ^ count
           -> Bool -- ^ no hide
           -> OutputFormat
           -> Bool -- ^ all status
           -> Maybe String -- ^ author
           -> Maybe String -- ^ since
           -> Maybe String -- ^ pattern
           -> String -- ^ repo
           -> IO ()
projectPRs server count nohide format allstatus mauthor msince mpat repo = do
  let namespace =
        if server == srcFedoraprojectOrg && '/' `notElem` repo
        then "rpms/" else ""
      project = namespace ++ repo
      path = project +/+ "pull-requests"
      params = [makeItem "status" "all" | allstatus] ++
               maybeKey "author" mauthor ++ maybeKey "since" msince
  pages <- queryPagureCountPaged server count path params ("pagination", "page")
  mapM_ (defaultPrinter nohide format (printPRs project)) pages
  where
    printPRs :: String -> Object -> IO ()
    printPRs project result = do
      let prs = lookupKey' "requests" result :: [Object]
      mapM_ (printPR project) prs

    printPR :: String -> Object -> IO ()
    printPR project pr = do
      case parsePR pr of
        Nothing -> putStrLn "parsing issue failed"
        Just (id',title,status) ->
          when (isNothing mpat || T.pack (fromJust mpat) `T.isInfixOf` title) $
          putStrLn $ "https://" ++ server +/+ project +/+ "pull-request" +/+ show id' ++ (if allstatus then " (" ++ lower (T.unpack status) ++ ")" else "") +-+ ":" +-+ T.unpack title

    parsePR :: Object -> Maybe (Integer, Text, Text)
    parsePR =
      parseMaybe $ \obj -> do
        id' <- obj .: "id"
        title <- obj .: "title"
        status <- obj .: "status"
        return (id',title,status)

-- | List repo git branches
repoBranches :: String -- ^ server
             -> Bool -- ^ no hide
             -> OutputFormat
             -> String -- ^ repo
             -> IO ()
repoBranches server nohide format repo = do
  let namespace =
        if server == srcFedoraprojectOrg && '/' `notElem` repo
        then "rpms/" else ""
      path = namespace ++ repo +/+ "git/branches"
  eres <- queryPagureSingle server path []
  either error' (defaultPrinter nohide format (printKeyList "branches")) eres

-- | List users matching pattern
users :: String -- ^ server
      -> Bool -- ^ no hide
      -> OutputFormat
      -> String -- ^ pattern
      -> IO ()
users server nohide format pat = do
  let path = "users"
      params = makeKey "pattern" pat
  res <- queryPagure server path params
  defaultPrinter nohide format (printKeyList "users") res

-- | Get full name of user
username :: String -- ^ server
         -> Bool -- ^ no hide
         -> OutputFormat
         -> String -- ^ user
         -> IO ()
username server nohide format user = do
  let path = "user" +/+ user
  eres <- queryPagureSingle server path $ makeKey "per_page" "1"
  case eres of
    Left err -> error' err
    Right res -> defaultPrinter nohide format printName res
  where
    printName res =
      case lookupKey "user" res >>= lookupKey "fullname" of
        Nothing -> error' "User fullname not found"
        Just fn -> T.putStrLn fn

-- | List groups with optional pattern
groups :: String -- ^ server
       -> Bool -- ^ count
       -> Bool -- ^ no hide
       -> OutputFormat
       -> Maybe String -- ^ pattern
       -> IO ()
groups server count nohide format mpat = do
  let path = "groups"
      params = maybeKey "pattern" mpat
  pages <- queryPagureCountPaged server count path params ("pagination", "page")
  mapM_ (defaultPrinter nohide format (printKeyList "groups")) pages

printKeyList :: String -> Object -> IO ()
printKeyList key' res =
  mapM_ T.putStrLn (lookupKey' (T.pack key') res :: [Text])

-- | Print git url of project
gitUrl :: String -- ^ server
       -> Bool -- ^ no hide
       -> OutputFormat
       -> String -- ^ repo
       -> IO ()
gitUrl server nohide format repo = do
  let namespace =
        if server == srcFedoraprojectOrg && '/' `notElem` repo
        then "rpms/" else ""
      path = namespace ++ repo +/+ "git/urls"
  res <- queryPagure server path []
  defaultPrinter nohide format printURLs res
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

-- | List contributors of project
contributors :: String -- ^ server
             -> Bool -- ^ no hide
             -> OutputFormat
             -> String -- ^ repo
             -> IO ()
contributors server nohide format repo = do
  let namespace =
        if server == srcFedoraprojectOrg && '/' `notElem` repo
        then "rpms/" else ""
      path = namespace ++ repo +/+ "contributors"
  res <- queryPagure server path []
  yamlPrinter nohide format res

-- | Yaml printer
yamlPrinter :: Bool -- ^ no hide
            -> OutputFormat
            -> Object
            -> IO ()
yamlPrinter nohide format =
  case format of
    FormatDefault -> yamlPrinter nohide FormatYaml
    FormatJson -> BL.putStrLn . encodePretty . hideEmpty
    FormatYaml -> B.putStrLn . encode . hideEmpty
  where
    hideEmpty = if nohide then id else cleanObject

    cleanObject :: Object -> Object
    cleanObject = M.mapMaybe go
      where
        go :: Value -> Maybe Value
        -- drop empty lists
        go (Array v) | V.null v = Nothing
                     | otherwise = Just $ Array (V.map (wrap go) v)
                     where
                       wrap f (Object o) = Object (M.mapMaybe f o)
                       wrap f (Array a)  = Array (V.map (wrap f) a)
                       wrap _ x          = x
        -- drop empty objects
        go (Object o) =
            let cleaned = M.mapMaybe go o
            in if M.null cleaned then Nothing else Just (Object cleaned)
        go other = Just other

-- | Show project info
projectInfo :: String -- ^ server
            -> Bool -- ^ no hide
            -> OutputFormat
            -> String -- ^ repo
            -> IO ()
projectInfo server nohide format repo = do
  let namespace =
        if server == srcFedoraprojectOrg && '/' `notElem` repo
        then "rpms/" else ""
      path = namespace ++ repo
  eval <- pagureProjectInfo server path
  either error' (yamlPrinter nohide format) eval

-- | Display project issue
projectIssue :: String -- ^ server
             -> Bool -- ^ no hide
             -> OutputFormat
             -> String -- ^ repo
             -> Int -- ^ issue
             -> IO ()
projectIssue server nohide format repo issue = do
  eval <- pagureProjectIssueInfo server repo issue
  either error' (yamlPrinter nohide format) eval

-- | Display project pull request
projectPR :: String -- ^ server
          -> Bool -- ^ no hide
          -> OutputFormat
          -> String -- ^ repo
          -> Int -- ^ PR number
          -> IO ()
projectPR server nohide format repo issue = do
  let namespace =
        if server == srcFedoraprojectOrg && '/' `notElem` repo
        then "rpms/" else ""
      project = namespace ++ repo
  eval <- pagureProjectPRInfo server project issue
  either error' (yamlPrinter nohide format) eval

-- | Display user info
userInfo :: String -- ^ server
         -> Bool -- ^ no hide
         -> OutputFormat
         -> String -- ^ user
         -> IO ()
userInfo server nohide format user = do
  eval <- pagureUserInfo server user []
  either error' (yamlPrinter nohide format) eval

-- | Show user activity stats
userActivityStats :: String -- ^ server
                  -> OutputFormat
                  -> String -- ^ user
                  -> IO ()
userActivityStats server format user = do
  eval <- pagureUserActivityStats server False user
  either error' (yamlPrinter False format) eval

-- | Show user activity on date
userActivityDate :: String -- ^ server
                 -> OutputFormat
                 -> Bool -- ^ summary
                 -> String -- ^ user
                 -> String -- ^ date
                 -> IO ()
userActivityDate server format summary user date = do
  eval <- pagureUserActivityDate server summary user date
  either error' (yamlPrinter False format) eval

-- | List user issues
userIssues :: String -- ^ server
           -> Bool -- ^ count
           -> OutputFormat
           -> String -- ^ user
           -> IO ()
userIssues server count format user = do
  issues <- pagureUserIssues server count user
  mapM_ (yamlPrinter False format) issues

-- | List pull requests filed by user
userRequestsFiled :: String -- ^ server
                  -> Bool -- ^ count
                  -> OutputFormat
                  -> Maybe String -- ^ pattern
                  -> Maybe String -- ^ status
                  -> String -- ^ user
                  -> IO ()
userRequestsFiled server count format mpat mstatus user = do
  result <- pagureUserRequestsFiled server count (fromMaybe "Open" mstatus) user
  let requests = concatMap (lookupKey' "requests") result
  mapM_ (defaultPrinter False format printPR) requests
  where
    printPR :: Object -> IO ()
    printPR pr = do
      case parsePR pr of
        Nothing -> putStrLn "parsing PR failed"
        Just (id',title,repo) ->
          when (isNothing mpat || T.pack (fromJust mpat) `T.isInfixOf` title) $
          putStrLn $ "https://" ++ server +/+ T.unpack repo +/+ "pull-request" +/+ show id' +-+ show (T.unpack title)

    parsePR :: Object -> Maybe (Integer, Text, Text)
    parsePR =
      parseMaybe $ \obj -> do
        id' <- obj .: "id"
        title <- obj .: "title"
        proj <- obj .: "project"
        repo <- proj .: "fullname"
        return (id',title,repo)

-- | Show actionable pull requests for user
userRequestsActionable :: String -- ^ server
                       -> Bool -- ^ count
                       -> OutputFormat
                       -> Maybe String -- ^ pattern
                       -> Maybe String -- ^ status
                       -> String -- ^ user
                       -> IO ()
userRequestsActionable server count format mpat mstatus user = do
  result <- pagureUserRequestsActionable server count (fromMaybe "Open" mstatus) user
  let requests = concatMap (lookupKey' "requests") result
  mapM_ (defaultPrinter False format printPR) requests
  where
    printPR :: Object -> IO ()
    printPR pr = do
      case parsePR pr of
        Nothing -> putStrLn "parsing PR failed"
        Just (id',title,repo) ->
          when (isNothing mpat || T.pack (fromJust mpat) `T.isInfixOf` title) $
          putStrLn $ "https://" ++ server +/+ T.unpack repo +/+ "pull-request" +/+ show id' +-+ show (T.unpack title)

    parsePR :: Object -> Maybe (Integer, Text, Text)
    parsePR =
      parseMaybe $ \obj -> do
        id' <- obj .: "id"
        title <- obj .: "title"
        proj <- obj .: "project"
        repo <- proj .: "fullname"
        return (id',title,repo)

-- | Show group info
groupInfo :: String -- ^ server
          -> Bool -- ^ projects
          -> Bool -- ^ no hide
          -> OutputFormat
          -> String -- ^ group
          -> IO ()
groupInfo server projects nohide format group = do
  let params = boolKey projects "projects" "1"
  eval <- pagureGroupInfo server group params
  either error' (yamlPrinter nohide format) eval

-- FIXME support acl parameter (admin, commit or ticket)
-- | List group projects
groupProjects :: String -- ^ server
              -> Bool -- ^ count
              -> String -- ^ group
              -> IO ()
groupProjects server count group =
  pagureGroupRepos server count group >>=
  mapM_ T.putStrLn
