module Main (main) where

import Data.Maybe (fromMaybe)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Lens.Micro
import Lens.Micro.Aeson
import SimpleCmdArgs

main :: IO ()
main = do
  simpleCmdArgs Nothing "Pagure client" "Simple pagure CLI" $
    subcommands
    [Subcommand "list" "list projects" $
     listProjects <$> strArg "PATTERN"]
    where
      listProjects :: String -> IO ()
      listProjects pat = do
        mgr <- newManager tlsManagerSettings
        let url = "https://src.fedoraproject.org/api/0/projects?namespace=rpms&fork=0&per_page=100&short=1&pattern=" ++ pat
        req1 <- parseRequest url
        res1 <- responseBody <$> httpLbs req1 mgr
        let pages = res1 ^? key "pagination" . key "pages" . _Integer
        mapM_ print $ res1 ^.. key "projects" . values . key "name" . _String
        mapM_ (nextPage mgr url) [2..(fromMaybe 0 pages)]
          where
            nextPage mgr url p = do
              req <- parseRequest $ url ++ "&page=" ++ show p
              res <- responseBody <$> httpLbs req mgr
              mapM_ print $ res ^.. key "projects" . values . key "name" . _String
