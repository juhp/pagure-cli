{-# LANGUAGE CPP #-}

module Main (main) where

import Control.Applicative (
#if !MIN_VERSION_base(4,8,0)
                            (<$>), (<*>),
#endif
#if !MIN_VERSION_simple_cmd_args(0,1,3)
                            (<|>)
#endif
                           )
import SimpleCmdArgs

import Fedora.Pagure.Client
import Paths_pagure_cli (version)

main :: IO ()
main =
  simpleCmdArgs (Just version) "Pagure client" "Simple pagure CLI" $
  subcommands
  [ Subcommand "list" "list projects" $
    listProjects
    <$> serverOpt
    <*> countOpt
    <*> nohideOpt
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
    <*> nohideOpt
    <*> formatOpt
    <*> strArg "PROJECT"
  , Subcommand "branches" "list project branches" $
    repoBranches
    <$> serverOpt
    <*> nohideOpt
    <*> formatOpt
    <*> strArg "REPO"
  , Subcommand "git-url" "show project repo's git urls" $
    gitUrl
    <$> serverOpt
    <*> nohideOpt
    <*> formatOpt
    <*> strArg "REPO"
  , Subcommand "contributors" "show project repo's contributors" $
    contributors
    <$> serverOpt
    <*> nohideOpt
    <*> formatOpt
    <*> strArg "REPO"
  , Subcommand "issues" "list project issues" $
    projectIssues
    <$> serverOpt
    <*> countOpt
    <*> nohideOpt
    <*> formatOpt
    <*> switchWith 'A' "all" "list Open and Closed issues"
    <*> optional (strOptionWith 'a' "author" "AUTHOR" "Filter issues by creator")
    <*> optional (strOptionWith 'S' "since" "Y-M-D" "Filter issues updated after date")
    <*> optional (strOptionWith 't' "title" "pattern" "Filter issues by title")
    <*> strArg "REPO"
  , Subcommand "issue" "show project issue" $
    projectIssue
    <$> serverOpt
    <*> nohideOpt
    <*> formatOpt
    <*> strArg "REPO"
    <*> argumentWith auto "ISSUE"
  , Subcommand "pull-requests" "list project pull requests" $
    projectPRs
    <$> serverOpt
    <*> countOpt
    <*> nohideOpt
    <*> formatOpt
    <*> switchWith 'A' "all" "list Open and Closed issues"
    <*> optional (strOptionWith 'a' "author" "AUTHOR" "Filter issues by creator")
    <*> optional (strOptionWith 'S' "since" "Y-M-D" "Filter issues updated after date")
    <*> optional (strOptionWith 't' "title" "pattern" "Filter issues by title")
    <*> strArg "REPO"
  , Subcommand "pull-request" "show project PR" $
    projectPR
    <$> serverOpt
    <*> nohideOpt
    <*> formatOpt
    <*> strArg "REPO"
    <*> argumentWith auto "ISSUE"
  , Subcommand "users" "list users" $
    users
    <$> serverOpt
    <*> nohideOpt
    <*> formatOpt
    <*> strArg "PATTERN"
  , Subcommand "username" "fullname of user" $
    username
    <$> serverOpt
    <*> nohideOpt
    <*> formatOpt
    <*> strArg "USERNAME"
  , Subcommand "userinfo" "show user details" $
    userInfo
    <$> serverOpt
    <*> nohideOpt
    <*> formatOpt
    <*> strArg "USERNAME"
  , Subcommand "user-activity-stats" "show user activity stats" $
    userActivityStats
    <$> serverOpt
    <*> formatOpt
    <*> strArg "USERNAME"
  , Subcommand "user-activity-date" "show user activity on date" $
    userActivityDate
    <$> serverOpt
    <*> formatOpt
    <*> switchWith 'S' "summary" "Summarize (group) activity per project"
    <*> strArg "USERNAME"
    <*> strArg "YYYY-MM-DD"
  , Subcommand "user-issues" "show user's issues" $
    userIssues
    <$> serverOpt
    <*> countOpt
    <*> formatOpt
    <*> strArg "USERNAME"
  , Subcommand "user-requests-filed" "show user's filed pull requests" $
    userRequestsFiled
    <$> serverOpt
    <*> countOpt
    <*> formatOpt
    <*> optional (strOptionWith 't' "title" "pattern" "Filter requests by title")
    <*> optional (strOptionWith 'S' "status" "open|closed|merged|all" "Select by status [default: Open]")
    <*> strArg "USERNAME"
  , Subcommand "user-requests-actionable" "show user's actionable pull requests" $
    userRequestsActionable
    <$> serverOpt
    <*> countOpt
    <*> formatOpt
    <*> optional (strOptionWith 't' "title" "pattern" "Filter requests by title")
    <*> optional (strOptionWith 'S' "status" "Open|Closed|Merged|All" "Select by status [default: Open]")
    <*> strArg "USERNAME"
  , Subcommand "groups" "list groups" $
    groups
    <$> serverOpt
    <*> countOpt
    <*> nohideOpt
    <*> formatOpt
    <*> optional (strArg "PATTERN")
  , Subcommand "groupinfo" "show group details" $
    groupInfo
    <$> serverOpt
    <*> switchWith 'p' "projects" "List projects"
    <*> nohideOpt
    <*> formatOpt
    <*> strArg "GROUP"
  ]
  where
    countOpt = switchWith 'c' "count" "Show number only"
    nohideOpt = switchWith 'e' "show-empty" "Display empty lists and objects"
    formatOpt = flagWith' FormatJson 'j' "json" "Output JSON" <|> flagWith FormatDefault FormatYaml 'y' "yaml" "YAML output"
    namespaceOpt = strOptionWith 'n' "namespace" "NAMESPACE" "Specify project repo namespace"
    packagerOpt = Owner <$> ownerOpt <|> Committer <$> usernameOpt
    usernameOpt = strOptionWith 'u' "username" "USERNAME" "Projects to which username can commit"
    ownerOpt = strOptionWith 'o' "owner" "OWNER" "Projects with certain owner"
    serverOpt = strOptionalWith 's' "server" "SERVER" "Pagure server" srcFedoraprojectOrg
    forksOpt = flagWith' OnlyForks 'F' "only-forks" "Only list forks" <|>
               flagWith NoForks IncludeForks 'f' "include-forks" "Include forks [default: ignore forks]"
