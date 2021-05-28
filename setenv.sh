#!/usr/bin/env bash

## Usage: $ . ./setenv.sh

##############################################################################
## Subroutines

getHome() {
    local source="${BASH_SOURCE[0]}"
    while [ -h "$source" ] ; do
        local linked="$(readlink "$source")"
        local dir="$( cd -P $(dirname "$source") && cd -P $(dirname "$linked") && pwd )"
        source="$dir/$(basename "$linked")"
    done
    ( cd -P "$(dirname "$source")" && pwd )
}

getOS() {
    local os
    case "$(uname -s)" in
        Linux*)  os=linux;;
        Darwin*) os=mac;;
        CYGWIN*) os=cygwin;;
        MINGW*)  os=mingw;;
        *)       os=unknown
    esac
    echo $os
}

getPath() {
    local path=""
    for i in $(ls -d "$1"*/ 2>/dev/null); do path=$i; done
    # ignore trailing slash introduced in for loop
    [[ -z "$path" ]] && echo "" || echo "${path::-1}"
}

##############################################################################
## Environment setup

PROG_HOME="$(getHome)"

OS="$(getOS)"
[[ $OS == "unknown" ]] && { echo "Unsuppored OS"; exit 1; }

if [[ $OS == "cygwin" || $OS == "mingw" ]]; then
    [[ $OS == "cygwin" ]] && prefix="/cygdrive" || prefix=""
    export HOME="$prefix/c/Users/$USER"
	export CABAL_DIR="$(getPath "$HOME/appdata/Roaming/cabal")"
    export GHC_HOME="$(getPath "$prefix/c/opt/ghc-8.10")"
    export GIT_HOME="$(getPath "$prefix/c/opt/Git-2")"
    export JAVA_HOME="$(getPath "$prefix/c/opt/jdk-openjdk-11")"
    export MAVEN_HOME="$(getPath "$prefix/c/opt/apache-maven-3")"
    export STACK_HOME="$(getPath "$prefix/c/opt/stack-2")"
else
    export CABAL_DIR="$(getPath "$HOME/~cabal")"
    export GHC_HOME="$(getPath "/opt/ghc-8.10")"
    export GIT_HOME="$(getPath "/opt/git-2")"
    export JAVA_HOME="$(getPath "/opt/jdk-openjdk-11")"
    export MAVEN_HOME="$(getPath "/opt/apache-maven-3")"
    export STACK_HOME="$(getPath "/opt/stack-2")"
fi
PATH1="$PATH"
[[ -x "$GIT_HOME/bin/git" ]] && PATH1="$PATH1:$GIT_HOME/bin"
[[ -x "$MAVEN_HOME/bin/mvn" ]] && PATH1="$PATH1:$MAVEN_HOME/bin"
[[ -x "$STACK_HOME/stack" ]] && PATH1="$PATH1:$STACK_HOME"
export PATH="$PATH1"
