#!/usr/bin/env bash
#
# Copyright (c) 2018-2024 Stéphane Micheloud
#
# Licensed under the MIT License.
#

##############################################################################
## Subroutines

getHome() {
    local source="${BASH_SOURCE[0]}"
    while [[ -h "$source" ]]; do
        local linked="$(readlink "$source")"
        local dir="$( cd -P $(dirname "$source") && cd -P $(dirname "$linked") && pwd )"
        source="$dir/$(basename "$linked")"
    done
    ( cd -P "$(dirname "$source")" && pwd )
}

debug() {
    local DEBUG_LABEL="[44m[DEBUG][0m"
    $DEBUG && echo "$DEBUG_LABEL $1" 1>&2
}

warning() {
    local WARNING_LABEL="[46m[WARNING][0m"
    echo "$WARNING_LABEL $1" 1>&2
}

error() {
    local ERROR_LABEL="[91mError:[0m"
    echo "$ERROR_LABEL $1" 1>&2
}

# use variables EXITCODE, TIMER_START
cleanup() {
    [[ $1 =~ ^[0-1]$ ]] && EXITCODE=$1

    if $TIMER; then
        local TIMER_END=$(date +'%s')
        local duration=$((TIMER_END - TIMER_START))
        echo "Total elapsed time: $(date -d @$duration +'%H:%M:%S')" 1>&2
    fi
    debug "EXITCODE=$EXITCODE"
    exit $EXITCODE
}

args() {
    [[ $# -eq 0 ]] && HELP=true && return 1

    for arg in "$@"; do
        case "$arg" in
        ## options
        -debug)    DEBUG=true ;;
        -help)     HELP=true ;;
        -timer)    TIMER=true ;;
        -verbose)  VERBOSE=true ;;
        -*)
            error "Unknown option $arg"
            EXITCODE=1 && return 0
            ;;
        ## subcommands
        clean)     CLEAN=true ;;
        compile)   COMPILE=true ;;
        doc)       DOC=true ;;
        help)      HELP=true ;;
        lint)      LINT=true ;;
        run)       COMPILE=true && RUN=true ;;
        *)
            error "Unknown subcommand $arg"
            EXITCODE=1 && return 0
            ;;
        esac
    done
    if $LINT; then
        if [[ ! -x "$HLINT_CMD" ]]; then
            warning "HLint installation not found"
            LINT=false
        fi
    fi
    debug "Properties : APP_NAME=$APP_NAME"
    debug "Options    : TIMER=$TIMER VERBOSE=$VERBOSE"
    debug "Subcommands: CLEAN=$CLEAN COMPILE=$COMPILE DOC=$DOC HELP=$HELP LINT=$LINT RUN=$RUN"
    debug "Variables  : CABAL_DIR=$CABAL_DIR"
    debug "Variables  : GHC_HOME=$GHC_HOME"
    # See http://www.cyberciti.biz/faq/linux-unix-formatting-dates-for-display/
    $TIMER && TIMER_START=$(date +"%s")
}

help() {
    cat << EOS
Usage: $BASENAME { <option> | <subcommand> }

  Options:
    -debug       print commands executed by this script
    -timer       print total execution time
    -verbose     print progress messages

  Subcommands:
    clean        delete generated files
    compile      compile Haskell source files
    doc          generate HTML documentation
    help         print this help message
    lint         analyze Scala source files with HLint
    run          execute main program "$APP_NAME"
EOS
}

clean() {
    if [[ -d "$TARGET_DIR" ]]; then
        if $DEBUG; then
            debug "Delete directory \"$TARGET_DIR\""
        elif $VERBOSE; then
            echo "Delete directory \"${TARGET_DIR/$ROOT_DIR\//}\"" 1>&2
        fi
        rm -rf "$TARGET_DIR"
        [[ $? -eq 0 ]] || ( EXITCODE=1 && return 0 )
    fi
}

lint() {
    [[ -d "$TARGET_DIR" ]] || mkdir "$TARGET_DIR"

    if $DEBUG; then set HLINT_OPTS=--color=auto
    else set HLINT_OPTS="--color=auto --report=\"$TARGET_DIR/report.html\""
    fi
    if $DEBUG; then debug "$HLINT_CMD $HLINT_OPTS $SOURCE_DIR"
    elif $VERBOSE; then echo "Analyze Haskell source files in directory \"${SOURCE_DIR/$ROOT_DIR/\\}\"" 1>&2
    fi
    eval "$HLINT_CMD" $HLINT_OPTS $SOURCE_DIR
    [[ $? -eq 0 ]] || ( EXITCODE=1 && return 0 )
}

compile() {
    [[ -d "$TARGET_DIR" ]] || mkdir -p "$TARGET_DIR"

    local required=$(action_required "$EXE_FILE" "$SOURCE_DIR/" "*.hs")
    [[ $required -eq 1 ]] || return 1

    local source_files=
    local n=0
    for f in $(find "$SOURCE_DIR/" -type f -name "*.hs" 2>/dev/null); do
        source_files="$source_files $f"
        n=$((n + 1))
    done
    if [[ $n -eq 0 ]]; then
        warning "No Haskell source file found"
        return 1
    fi
    local s=; [[ $n -gt 1 ]] && s="s"
    local n_files="$n Haskell source file$s"

    if $DEBUG; then debug "$GHC_CMD $GHC_OPTS $source_files"
    elif $VERBOSE; then echo "Compile $n_files to file \"${EXE_FILE/$ROOT_DIR/\\}\"" 1>&2
    fi
    eval "$GHC_CMD" $GHC_OPTS $source_files
    if [[ $? -ne 0 ]]; then
        error "Failed to compile $n_files to file \"${EXE_FILE/$ROOT_DIR/\\}\"" 1>&2
        cleanup 1
    fi
}

mixed_path() {
    if [[ -x "$CYGPATH_CMD" ]]; then
        $CYGPATH_CMD -am $1
    elif $mingw || $msys; then
        echo $1 | sed 's|/|\\\\|g'
    else
        echo $1
    fi
}

action_required() {
    local target_file=$1
    local search_path=$2
    local search_pattern=$3
    local source_file=
    for f in $(find "$search_path" -type f -name "$search_pattern" 2>/dev/null); do
        [[ $f -nt $source_file ]] && source_file=$f
    done
    if [[ -z "$source_file" ]]; then
        ## Do not compile if no source file
        echo 0
    elif [[ ! -f "$target_file" ]]; then
        ## Do compile if target file doesn't exist
        echo 1
    else
        ## Do compile if target file is older than most recent source file
        [[ $target_file -nt $source_file ]] && echo 1 || echo 0
    fi
}

doc() {
    [[ -d "$TARGET_DOCS_DIR" ]] || mkdir -p "$TARGET_DOCS_DIR"

    local html_libs_dir="$GHC_HOME/doc/html/libraries"
    if [[ ! -d "$html_libs_dir" ]]; then
        echo error "GHC HTML documentation directory not found" 1>&2
        cleanup 1
    fi
    local haddock_opts="$HADDOCK_OPTS --title=$PROJECT_NAME --package-name=$APP_NAME --package-version=$PROJECT_VERSION"
    ## Use "*.haddock" instead of "base.haddock" to include all interface docs.
    for f in $(find "$html_libs_dir" -type f -name "base.haddock"); do
        file="$(mixed_path $f)"
		file="${GHC_HOME}\\doc\\html\\libraries\\base-4.14.3.0\\base.haddock"
        parent_dir="$(dirname $file)/"
		parent_dir="C${GHC_HOME}\\doc\\html\\libraries\\base-4.14.3.0"
        haddock_opts="$haddock_opts --read-interface=\"$parent_dir\",\"$file\""
    done
    local source_files=
    for f in $(find $SOURCE_DIR/ -type f -name "*.hs" 2>/dev/null); do
        source_files="$source_files $(mixed_path $f)"
    done
    if $DEBUG; then debug "$HADDOCK_CMD $haddock_opts $source_files"
    elif $VERBOSE; then echo "Generate HTML documentation into directory \"${TARGET_DOCS_DIR/$ROOT_DIR/\\}\"" 1>&2
    fi
    eval "$HADDOCK_CMD" $haddock_opts $source_files
    if [[ $? -ne 0 ]]; then
        error "Failed to generate HTML documentation into directory \"${TARGET_DOCS_DIR/$ROOT_DIR/\\}\""
        cleanup 1
    fi
}

run() {
    if [[ ! -f "$EXE_FILE" ]]; then
        error "Executable not found (\"${EXE_FILE/$ROOT_DIR/\\}\")"
        cleanup 1
    fi
    if $DEBUG; then debug "$EXE_FILE"
    elif $VERBOSE; then echo "Execute Haskell program \"${EXE_FILE/$ROOT_DIR/\\}\"" 1>&2
    fi
    eval "$EXE_FILE"
    if [[ $? -ne 0 ]]; then
        error "Program executable not found (\"${EXE_FILE/$ROOT_DIR/\\}\")"
        cleanup 1
    fi
}

##############################################################################
## Environment setup

BASENAME=$(basename "${BASH_SOURCE[0]}")

EXITCODE=0

ROOT_DIR="$(getHome)"

SOURCE_DIR="$ROOT_DIR/app"
TARGET_DIR="$ROOT_DIR/target"
TARGET_DOCS_DIR="$TARGET_DIR/docs"
TARGET_GEN_DIR="$TARGET_DIR/gen"

CLEAN=false
COMPILE=false
DEBUG=false
DOC=false
HELP=false
LINT=false
RUN=false
TEST=false
TIMER=false
VERBOSE=false

COLOR_START="[32m"
COLOR_END="[0m"

APP_NAME=ArithmeticExpr
EXE_FILE="$TARGET_DIR/$APP_NAME.exe"

cygwin=false
mingw=false
msys=false
darwin=false
case "$(uname -s)" in
    CYGWIN*) cygwin=true ;;
    MINGW*)  mingw=true ;;
    MSYS*)   msys=true ;;
    Darwin*) darwin=true
esac
unset CYGPATH_CMD
PSEP=":"
if $cygwin || $mingw || $msys; then
    CYGPATH_CMD="$(which cygpath 2>/dev/null)"
    PSEP=";"
    [[ -n "$CABAL_DIR" ]] && CABAL_DIR="$(mixed_path $CABAL_DIR)"
    [[ -n "$GHC_HOME" ]] && GHC_HOME="$(mixed_path $GHC_HOME)"
    [[ -n "$GIT_HOME" ]] && GIT_HOME="$(mixed_path $GIT_HOME)"
    DIFF_CMD="$GIT_HOME/usr/bin/diff.exe"
else
    DIFF_CMD="$(which diff)"
fi
if [[ ! -x "$GHC_HOME/bin/ghc.exe" ]]; then
    error "GHC installation not found"
    cleanup 1
fi
GHC_CMD="$GHC_HOME/bin/ghc.exe"
## option "-hidir <dir>" redirects all generated interface files into <dir>
GHC_OPTS="-Wall -Werror -hidir $TARGET_GEN_DIR -odir $TARGET_GEN_DIR -o $EXE_FILE"

HADDOCK_CMD="$GHC_HOME/bin/haddock.exe"
HADDOCK_OPTS="--html --odir=$(mixed_path $TARGET_DOCS_DIR)"

HLINT_CMD=
[[ -x "$CABAL_DIR/bin/hlint" ]] && HLINT_CMD="$CABAL_DIR/bin/hlint"

PROJECT_NAME="$(basename $ROOT_DIR)"
PROJECT_URL="github.com/$USER/haskell-examples"
PROJECT_VERSION="1.0-SNAPSHOT"

args "$@"
[[ $EXITCODE -eq 0 ]] || cleanup 1

##############################################################################
## Main

$HELP && help && cleanup

if $CLEAN; then
    clean || cleanup 1
fi
if $LINT; then
    lint || cleanup 1
fi
if $COMPILE; then
    compile || cleanup 1
fi
if $DOC; then
    doc || cleanup 1
fi
if $RUN; then
    run || cleanup 1
fi
if $TEST; then
    run_tests || cleanup 1
fi
cleanup
