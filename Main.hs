{-# LANGUAGE CPP #-}

module Main (main) where

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,0))
#else
import Control.Applicative ((<$>), (<*>))
#endif
#if (defined(MIN_VERSION_simple_cmd_args) && MIN_VERSION_simple_cmd_args(0,1,3))
#else
import Control.Applicative ((<|>))
#endif
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
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Lens.Micro
import Lens.Micro.Aeson
import SimpleCmdArgs

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
  listPagure server count detail showurl query ("pagination", "page", "projects")
  where
    render :: Filter -> String
    -- hack until internal server API upgraded to >=0.29
    render (All s) = (if ".redhat.com" `isSuffixOf` server then "" else "owner=!orphan&") <> "pattern=" <> s
    render (Owner n mpat) = "owner=" <> n <> maybe "" ("&pattern=" <>) mpat

userRepos :: String -> Bool -> Bool -> Bool -> String -> IO ()
userRepos server count detail showurl user = do
  let query = "user/" <> user <> "?"
  listPagure server count detail showurl query ("repos_pagination", "repopage", "repos")

listPagure :: String -> Bool -> Bool -> Bool -> String -> (String,String,String) -> IO ()
listPagure server count detail showurl query (pagination,paging,object) = do
  let url = "https://" <> server <> "/api/0/" <> query <> "per_page=" <> if count then "1" else "100"
  when showurl $ putStrLn url
  req1 <- parseRequest url
  mgr <- newManager tlsManagerSettings
  res1 <- responseBody <$> httpLbs req1 mgr
  if detail then B.putStrLn res1
    else do
    let pages = res1 ^? key (T.pack pagination) . key "pages" . _Integer
    if count
      then print $ fromMaybe (error "not found") pages
      else do
      printRepos res1
      mapM_ (nextPage mgr url) [2..(fromMaybe 0 pages)]
     where
        nextPage mgr url p = do
          req <- parseRequest $ url <> "&" <> paging <> "=" <> show p
          res <- responseBody <$> httpLbs req mgr
          printRepos res

        printRepos res =
          mapM_ T.putStrLn $ res ^.. key (T.pack object) . values . key "name" . _String
