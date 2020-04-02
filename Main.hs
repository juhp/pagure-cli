{-# LANGUAGE CPP #-}

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
import Data.Maybe (fromMaybe)
#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,11,0))
#else
import Data.Semigroup ((<>))
#endif
import Data.List (isSuffixOf)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Network.HTTP.Simple
import Lens.Micro
import Lens.Micro.Aeson
import SimpleCmdArgs
import System.FilePath ((</>))

import Paths_pagure_cli (version)

data Filter = Owner String (Maybe String) | All String

main :: IO ()
main =
  simpleCmdArgs (Just version) "Pagure client" "Simple pagure CLI" $
  subcommands
  [ Subcommand "list" "list projects" $
    listProjects <$> serverOpt <*> countOpt <*> detailsOpt <*> urlOpt <*> searchFilter
  , Subcommand "user" "user repos" $
    userRepos <$> serverOpt <*> countOpt <*> detailsOpt <*> urlOpt <*> strArg "USER"
  , Subcommand "branches" "show project branches" $
    repoBranches <$> serverOpt <*> detailsOpt <*> urlOpt <*> strArg "REPO"
  ]
  where
    countOpt = switchWith 'c' "count" "Show number only"
    detailsOpt = switchWith 'd' "detail" "Show all details"
    urlOpt = switchWith 'u' "url" "Show url"
    ownerOpt = strOptionWith 'o' "owner" "OWNER" "Projects with certain owner"
    serverOpt = strOptionalWith 's' "server" "SERVER" "Pagure server" "src.fedoraproject.org"
    searchFilter = All <$> strArg "PATTERN" <|>
                   Owner <$> ownerOpt <*> optional (strArg "PATTERN")

listProjects :: String -> Bool -> Bool -> Bool -> Filter -> IO ()
listProjects server count detail showurl search = do
  let query = "projects?namespace=rpms&fork=0&" <> render search <> "&"
  queryPaged server count detail showurl query ("pagination", "page", "projects")
  where
    render :: Filter -> String
    -- hack until internal server API upgraded to >=0.29
    render (All s) = (if ".redhat.com" `isSuffixOf` server then "" else "owner=!orphan&") <> "pattern=" <> s
    render (Owner n mpat) = "owner=" <> n <> maybe "" ("&pattern=" <>) mpat

userRepos :: String -> Bool -> Bool -> Bool -> String -> IO ()
userRepos server count detail showurl user = do
  let query = "user" </> user <> "?"
  queryPaged server count detail showurl query ("repos_pagination", "repopage", "repos")

queryPaged :: String -> Bool -> Bool -> Bool -> String -> (String,String,String) -> IO ()
queryPaged server count detail showurl query (pagination,paging,object) = do
  let url = "https://" <> server </> "api/0" </> query <> "per_page=" <> if count then "1" else "100"
  res1 <- pagureQuery showurl url
  if detail then B.putStrLn res1
    else do
    let pages = res1 ^? key (T.pack pagination) . key "pages" . _Integer
    if count
      then print $ fromMaybe (error "not found") pages
      else do
      printRepos res1
      mapM_ (nextPage url) [2..(fromMaybe 0 pages)]
     where
        nextPage url p = do
          res <- pagureQuery False $ url <> "&" <> paging <> "=" <> show p
          printRepos res

        printRepos res =
          mapM_ T.putStrLn $ res ^.. key (T.pack object) . values . key "name" . _String

pagureQuery :: Bool -> String -> IO B.ByteString
pagureQuery showurl url = do
  when showurl $ putStrLn url
  req <- parseRequest url
  getResponseBody <$> httpLBS req

repoBranches :: String -> Bool -> Bool -> String -> IO ()
repoBranches server detail showurl repo = do
  let query = "rpms" </> repo </> "git/branches"
  querySingle server detail showurl query

querySingle :: String -> Bool -> Bool -> String -> IO ()
querySingle server detail showurl query = do
  let url = "https://" <> server </> "api/0" </> query
  res <- pagureQuery showurl url
  if detail then B.putStrLn res
    else
    printRepos res
  where
    printRepos res =
      mapM_ T.putStrLn $ res ^.. key "branches" . values . _String
