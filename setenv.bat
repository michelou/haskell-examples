@echo off
setlocal enabledelayedexpansion

@rem only for interactive debugging
set _DEBUG=0

@rem #########################################################################
@rem ## Environment setup

set _EXITCODE=0

call :env
if not %_EXITCODE%==0 goto end

call :args %*
if not %_EXITCODE%==0 goto end

@rem #########################################################################
@rem ## Main

if %_HELP%==1 (
    call :help
    exit /b !_EXITCODE!
)

set _GHC_PATH=
set _GIT_PATH=

call :ghc
if not %_EXITCODE%==0 goto end

call :git
if not %_EXITCODE%==0 goto end

goto end

@rem #########################################################################
@rem ## Subroutines

@rem output parameters: _DEBUG_LABEL, _ERROR_LABEL, _WARNING_LABEL
:env
set _BASENAME=%~n0
set "_ROOT_DIR=%~dp0"

call :env_colors
set _DEBUG_LABEL=%_NORMAL_BG_CYAN%[%_BASENAME%]%_RESET%
set _ERROR_LABEL=%_STRONG_FG_RED%Error%_RESET%:
set _WARNING_LABEL=%_STRONG_FG_YELLOW%Warning%_RESET%:
set _PS1_LABEL=%_STRONG_BG_YELLOW%PS1%_RESET%

set "_HASH_DIR=%_ROOT_DIR%.hash"
goto :eof

:env_colors
@rem ANSI colors in standard Windows 10 shell
@rem see https://gist.github.com/mlocati/#file-win10colors-cmd
set _RESET=[0m
set _BOLD=[1m
set _UNDERSCORE=[4m
set _INVERSE=[7m

@rem normal foreground colors
set _NORMAL_FG_BLACK=[30m
set _NORMAL_FG_RED=[31m
set _NORMAL_FG_GREEN=[32m
set _NORMAL_FG_YELLOW=[33m
set _NORMAL_FG_BLUE=[34m
set _NORMAL_FG_MAGENTA=[35m
set _NORMAL_FG_CYAN=[36m
set _NORMAL_FG_WHITE=[37m

@rem normal background colors
set _NORMAL_BG_BLACK=[40m
set _NORMAL_BG_RED=[41m
set _NORMAL_BG_GREEN=[42m
set _NORMAL_BG_YELLOW=[43m
set _NORMAL_BG_BLUE=[44m
set _NORMAL_BG_MAGENTA=[45m
set _NORMAL_BG_CYAN=[46m
set _NORMAL_BG_WHITE=[47m

@rem strong foreground colors
set _STRONG_FG_BLACK=[90m
set _STRONG_FG_RED=[91m
set _STRONG_FG_GREEN=[92m
set _STRONG_FG_YELLOW=[93m
set _STRONG_FG_BLUE=[94m
set _STRONG_FG_MAGENTA=[95m
set _STRONG_FG_CYAN=[96m
set _STRONG_FG_WHITE=[97m

@rem strong background colors
set _STRONG_BG_BLACK=[100m
set _STRONG_BG_RED=[101m
set _STRONG_BG_GREEN=[102m
set _STRONG_BG_YELLOW=[103m
set _STRONG_BG_BLUE=[104m
goto :eof

:env_uptodate
call :check_hash "Cabal" "https://www.haskell.org/cabal/download.html"
call :check_hash "GHC" "https://downloads.haskell.org/~ghc/latest/"
call :check_hash "hlint" "https://hackage.haskell.org/package/hlint/changelog"
call :check_hash "hpack" "https://hackage.haskell.org/package/hpack/changelog"
@rem constain tags, e.g. <div ... id="tags-menu-78c63780-ae14-11ea-8f6b-9333f0f9ebed">
@rem call :check_hash "stack" "https://github.com/commercialhaskell/stack/releases"
goto :eof

@rem input parameters: %1=name, %2=URL
:check_hash
set __NAME=%~1
set __URL=%~2

set "__HASH_FILE=%_HASH_DIR%\%__NAME%_sh256.txt"

if %_DEBUG%==1 ( echo %_DEBUG_LABEL% %_PS1_LABEL% Get-FileHash -InputStream^($wc.OpenRead^(%__URL%^)^) 1>&2
) else if %_VERBOSE%==1 ( echo Compute hash code for URL %__URL% 1>&2
)
for /f "usebackq" %%i in (`powershell -C "$wc=[System.Net.WebClient]::new();$url='%__URL%';$fh=Get-FileHash -InputStream($wc.OpenRead($url));$fh.Hash"`) do (
     set __FILE_HASH=%%i
)
if not exist "%__HASH_FILE%" (
    if not exist "%_HASH_DIR%" mkdir "%_HASH_DIR%"
    if %_DEBUG%==1 ( echo %_DEBUG_LABEL% Create %__NAME% hash file 1>&2
    ) else if %_VERBOSE%==1 ( echo Create %__NAME% hash file 1>&2
    )
    (
        echo # %__URL%
        echo %__FILE_HASH%
    ) > "%__HASH_FILE%"
) else (
    for /f "delims=" %%i in (%__HASH_FILE%) do (
        set "__LINE=%%i"
        if not "!__LINE:~0,1!"=="#" set __CHECK_HASH=%%i
    )
    if not "%__FILE_HASH%"=="!__CHECK_HASH!" (
        echo %_WARNING_LABEL% Change detected on page %__URL% 1>&2
        if %_DEBUG%==1 (
            echo %_DEBUG_LABEL% __FILE_HASH=%__FILE_HASH% 1>&2
            echo %_DEBUG_LABEL% __CHECK_HASH=!__CHECK_HASH! 1>&2
        )
    )
)
goto :eof

@rem input parameter: %*
:args
set _HELP=0
set _VERBOSE=0
set __N=0
:args_loop
set "__ARG=%~1"
if not defined __ARG goto args_done

if "%__ARG:~0,1%"=="-" (
    @rem option
    if /i "%__ARG%"=="-debug" ( set _DEBUG=1
    ) else if /i "%__ARG%"=="-verbose" ( set _VERBOSE=1
    ) else (
        echo %_ERROR_LABEL% Unknown option %__ARG% 1>&2
        set _EXITCODE=1
        goto args_done
    )
) else (
    @rem subcommand
    if /i "%__ARG%"=="help" ( set _HELP=1
    ) else (
        echo %_ERROR_LABEL% Unknown subcommand %__ARG% 1>&2
        set _EXITCODE=1
        goto args_done
    )
    set /a __N+=1
)
shift
goto :args_loop
:args_done
if %_DEBUG%==1 (
    call :env_uptodate
    if not !_EXITCODE!==0 goto end
)
if %_DEBUG%==1  echo %_DEBUG_LABEL% _HELP=%_HELP% _VERBOSE=%_VERBOSE% 1>&2
goto :eof

:help
if %_VERBOSE%==1 (
    set __BEG_P=%_STRONG_FG_CYAN%%_UNDERSCORE%
    set __BEG_O=%_STRONG_FG_GREEN%
    set __BEG_N=%_NORMAL_FG_YELLOW%
    set __END=%_RESET%
) else (
    set __BEG_P=
    set __BEG_O=
    set __BEG_N=
    set __END=
)
echo Usage: %__BEG_O%%_BASENAME% { ^<option^> ^| ^<subcommand^> }%__END%
echo.
echo   %__BEG_P%Options:%__END%
echo     %__BEG_O%-debug%__END%      show commands executed by this script
echo     %__BEG_O%-verbose%__END%    display environment settings
echo.
echo   %__BEG_P%Subcommands:%__END%
echo     %__BEG_O%help%__END%        display this help message
goto :eof

@rem output parameter(s): _GHC_HOME, _GHC_PATH
:ghc
set _GHC_HOME=
set _GHC_PATH=

set __GHC_CMD=
for /f %%f in ('where ghc.exe 2^>NUL') do set "__GHC_CMD=%%f"
if defined __GHC_CMD (
    if %_DEBUG%==1 echo %_DEBUG_LABEL% Using path of Haskell executable found in PATH 1>&2
    for %%i in ("%__GHC_CMD%") do set "__GHC_BIN_DIR=%%~dpi"
    for %%f in ("!__GHC_BIN_DIR!..") do set "_GHC_HOME=%%~dpf"
    @rem keep _GHC_PATH undefined since executable already in path
    goto :eof
) else if defined HASKELL_HOME (
    set "_GHC_HOME=%HASKELL_HOME%"
    if %_DEBUG%==1 echo %_DEBUG_LABEL% Using environment variable HASKELL_HOME 1>&2
) else (
    set __PATH=C:\opt
    for /f %%f in ('dir /ad /b "!__PATH!\ghc-8*" 2^>NUL') do set "_GHC_HOME=!__PATH!\%%f"
    if not defined _GHC_HOME (
        set "__PATH=%ProgramFiles%"
        for /f "delims=" %%f in ('dir /ad /b "!__PATH!\ghc-8*" 2^>NUL') do set "_GHC_HOME=!__PATH!\%%f"
    )
)
if not exist "%_GHC_HOME%\bin\ghc.exe" (
    echo %_ERROR_LABEL% Executable ghc.exe not found ^(%_GHC_HOME%^) 1>&2
    set _EXITCODE=1
    goto :eof
)
set __HLINT_PATH=
if exist "%_GHC_HOME%\hlint\bin\hlint.exe" ( set "__HLINT_PATH=;%_GHC_HOME%\hlint\bin"
) else ( echo %_WARNING_LABEL% HLint tool not installed 1>&2
)
set __HPACK_PATH=
if exist "%_GHC_HOME%\hpack\bin\hpack.exe" ( set "__HPACK_PATH=;%_GHC_HOME%\hpack\bin"
) else ( echo %_WARNING_LABEL% HPack tool not installed 1>&2
)
set __STACK_PATH=
if exist "%_GHC_HOME%\stack\stack.exe" ( set "__STACK_PATH=;%_GHC_HOME%\stack"
) else ( echo %_WARNING_LABEL% Stack tool not installed 1>&2
)
set "_GHC_PATH=;%_GHC_HOME%\bin%__HLINT_PATH%%__HPACK_PATH%%__STACK_PATH%"
goto :eof

@rem output parameter(s): _GIT_HOME, _GIT_PATH
:git
set _GIT_HOME=
set _GIT_PATH=

set __GIT_CMD=
for /f %%f in ('where git.exe 2^>NUL') do set "__GIT_CMD=%%f"
if defined __GIT_CMD (
    if %_DEBUG%==1 echo %_DEBUG_LABEL% Using path of Git executable found in PATH 1>&2
    @rem keep _GIT_PATH undefined since executable already in path
    goto :eof
) else if defined GIT_HOME (
    set "_GIT_HOME=%GIT_HOME%"
    if %_DEBUG%==1 echo %_DEBUG_LABEL% Using environment variable GIT_HOME 1>&2
) else (
    set __PATH=C:\opt
    if exist "!__PATH!\Git\" ( set "_GIT_HOME=!__PATH!\Git"
    ) else (
        for /f %%f in ('dir /ad /b "!__PATH!\Git*" 2^>NUL') do set "_GIT_HOME=!__PATH!\%%f"
        if not defined _GIT_HOME (
            set "__PATH=%ProgramFiles%"
            for /f %%f in ('dir /ad /b "!__PATH!\Git*" 2^>NUL') do set "_GIT_HOME=!__PATH!\%%f"
        )
    )
)
if not exist "%_GIT_HOME%\bin\git.exe" (
    echo %_ERROR_LABEL% Git executable not found ^(%_GIT_HOME%^) 1>&2
    set _EXITCODE=1
    goto :eof
)
set "_GIT_PATH=;%_GIT_HOME%\bin;%_GIT_HOME%\mingw64\bin;%_GIT_HOME%\usr\bin"
goto :eof

:print_env
set __VERBOSE=%1
set "__VERSIONS_LINE1=  "
set "__VERSIONS_LINE2=  "
set "__VERSIONS_LINE3=  "
set __WHERE_ARGS=
where /q cabal.exe
if %ERRORLEVEL%==0 (
    for /f "tokens=1,2,*" %%i in ('cabal.exe --version ^| findstr install') do set "__VERSIONS_LINE1=%__VERSIONS_LINE1% cabal %%~k,"
    set __WHERE_ARGS=%__WHERE_ARGS% cabal.exe
)
where /q ghc.exe
if %ERRORLEVEL%==0 (
    for /f "delims=, tokens=1,*" %%i in ('ghc.exe --version') do set "__VERSIONS_LINE1=%__VERSIONS_LINE1% ghc%%~j,"
    set __WHERE_ARGS=%__WHERE_ARGS% ghc.exe
)
where /q stack.exe
if %ERRORLEVEL%==0 (
    for /f "tokens=1,2,*" %%i in ('stack.exe --version') do set "__VERSIONS_LINE1=%__VERSIONS_LINE1% stack %%~j"
    set __WHERE_ARGS=%__WHERE_ARGS% stack.exe
)
where /q haddock.exe
if %ERRORLEVEL%==0 (
    for /f "tokens=1,2,3,*" %%i in ('haddock.exe --version ^| findstr version') do set "__VERSIONS_LINE2=%__VERSIONS_LINE2% haddock %%~k"
    set __WHERE_ARGS=%__WHERE_ARGS% haddock.exe
)
where /q hlint.exe
if %ERRORLEVEL%==0 (
    for /f "tokens=1,2,*" %%i in ('hlint.exe --version') do set "__VERSIONS_LINE2=%__VERSIONS_LINE2% hlint %%~j"
    set __WHERE_ARGS=%__WHERE_ARGS% hlint.exe
)
where /q hpack.exe
if %ERRORLEVEL%==0 (
    for /f "tokens=1,2,*" %%i in ('hpack.exe --version') do set "__VERSIONS_LINE2=%__VERSIONS_LINE2% hpack %%~k,"
    set __WHERE_ARGS=%__WHERE_ARGS% hpack.exe
)
where /q pandoc.exe
if %ERRORLEVEL%==0 (
    for /f "tokens=1,*" %%i in ('pandoc.exe --version ^| findstr /b pandoc') do set "__VERSIONS_LINE2=%__VERSIONS_LINE2% pandoc %%~j,"
    set __WHERE_ARGS=%__WHERE_ARGS% pandoc.exe
)
where /q git.exe
if %ERRORLEVEL%==0 (
    for /f "tokens=1,2,*" %%i in ('git.exe --version') do set "__VERSIONS_LINE3=%__VERSIONS_LINE3% git %%k,"
    set __WHERE_ARGS=%__WHERE_ARGS% git.exe
)
where /q diff.exe
if %ERRORLEVEL%==0 (
   for /f "tokens=1-3,*" %%i in ('diff.exe --version ^| findstr /B diff') do set "__VERSIONS_LINE3=%__VERSIONS_LINE3% diff %%l"
    set __WHERE_ARGS=%__WHERE_ARGS% diff.exe
)
echo Tool versions:
echo %__VERSIONS_LINE1%
echo %__VERSIONS_LINE2%
echo %__VERSIONS_LINE3%
if %__VERBOSE%==1 if defined __WHERE_ARGS (
    @rem if %_DEBUG%==1 echo %_DEBUG_LABEL% where %__WHERE_ARGS%
    echo Tool paths: 1>&2
    for /f "tokens=*" %%p in ('where %__WHERE_ARGS%') do echo    %%p 1>&2
    echo Environment variables: 1>&2
    if defined GHC_HOME echo    GHC_HOME=%GHC_HOME% 1>&2
    if defined STACK_WORK echo    STACK_WORK=%STACK_WORK% 1>&2
)
goto :eof

@rem #########################################################################
@rem ## Cleanups

:end
endlocal & (
    if not defined GHC_HOME set "GHC_HOME=%_GHC_HOME%"
    for /f %%i in ('stack.exe --version 2^>NUL') do set STACK_WORK=target
    set "PATH=%PATH%%_GHC_PATH%%_GIT_PATH%"
    call :print_env %_VERBOSE%
    if %_DEBUG%==1 echo %_DEBUG_LABEL% _EXITCODE=%_EXITCODE% 1>&2
    for /f "delims==" %%i in ('set ^| findstr /b "_"') do set %%i=
)
