{-# LANGUAGE CPP #-}

module Main (main) where

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,0))
#else
import Control.Applicative ((<$>), (<*>))
#endif
import Data.Maybe (fromMaybe)
#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,11,0))
#else
import Data.Semigroup ((<>))
#endif
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text.IO as T
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Lens.Micro
import Lens.Micro.Aeson
import SimpleCmdArgs

import Paths_pagure_cli (version)

main :: IO ()
main =
  simpleCmdArgs (Just version) "Pagure client" "Simple pagure CLI" $
    subcommands
    [Subcommand "list" "list projects" $
     listProjects <$> detailsOpt <*> ownerOpt <*> strArg "PATTERN"]
    where
      detailsOpt = switchWith 'd' "detail" "Show all details"
      ownerOpt = strOptionalWith 'o' "owner" "OWNER" "Projects with certain owner" "!orphan"

      listProjects :: Bool -> String -> String -> IO ()
      listProjects detail owner pat = do
        mgr <- newManager tlsManagerSettings
        let url = "https://src.fedoraproject.org/api/0/projects?namespace=rpms&fork=0&per_page=100" <> "&owner=" <> owner <> "&pattern=" <> pat
        req1 <- parseRequest url
        res1 <- responseBody <$> httpLbs req1 mgr
        if detail then B.putStrLn res1
          else do
          let pages = res1 ^? key "pagination" . key "pages" . _Integer
          printProjects res1
          mapM_ (nextPage mgr url) [2..(fromMaybe 0 pages)]
            where
              nextPage mgr url p = do
                req <- parseRequest $ url <> "&page=" <> show p
                res <- responseBody <$> httpLbs req mgr
                printProjects res

              printProjects res =
                mapM_ T.putStrLn $ res ^.. key "projects" . values . key "name" . _String
