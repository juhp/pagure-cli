#!/bin/bash

stack --silent install

pagure=$HOME/.local/bin/pagure

function run {
    echo ">> pagure $1"
    out=$(pagure $1)
    if [[ "$out" = "$2" ]]; then
        echo "pass: pagure $1"
    else
        echo "fail: pagure $1:"
        echo "output: $out"
        echo "expect: $2"
    fi
}

run "list coreutils" "rpms/coreutils"
run "list -n rpms gcc" "gcc"
run "list -s pagure.io pagure" "pagure"
