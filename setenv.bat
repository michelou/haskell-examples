@echo off
setlocal enabledelayedexpansion

rem only for interactive debugging
set _DEBUG=0

rem ##########################################################################
rem ## Environment setup

set _BASENAME=%~n0

set _EXITCODE=0

for %%f in ("%~dp0") do set _ROOT_DIR=%%~sf

call :env
if not %_EXITCODE%==0 goto end

call :args %*
if not %_EXITCODE%==0 goto end

rem ##########################################################################
rem ## Main

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

rem ##########################################################################
rem ## Subroutines

rem output parameters: _DEBUG_LABEL, _ERROR_LABEL, _WARNING_LABEL
:env
rem ANSI colors in standard Windows 10 shell
rem see https://gist.github.com/mlocati/#file-win10colors-cmd
set _DEBUG_LABEL=[46m[%_BASENAME%][0m
set _ERROR_LABEL=[91mError[0m:
set _WARNING_LABEL=[93mWarning[0m:

rem for %%f in ("%ProgramFiles%") do set _PROGRAM_FILES=%%~sf
goto :eof

rem input parameter: %*
:args
set _HELP=0
set _VERBOSE=0
set __N=0
:args_loop
set "__ARG=%~1"
if not defined __ARG goto args_done

if "%__ARG:~0,1%"=="-" (
    rem option
    if /i "%__ARG%"=="-debug" ( set _DEBUG=1
    ) else if /i "%__ARG%"=="-verbose" ( set _VERBOSE=1
    ) else (
        echo %_ERROR_LABEL% Unknown option %__ARG% 1>&2
        set _EXITCODE=1
        goto args_done
    )
) else (
    rem subcommand
    set /a __N+=1
    if /i "%__ARG%"=="help" ( set _HELP=1
    ) else (
        echo %_ERROR_LABEL% Unknown subcommand %__ARG% 1>&2
        set _EXITCODE=1
        goto args_done
    )
)
shift
goto :args_loop
:args_done
if %_DEBUG%==1 echo %_DEBUG_LABEL% _HELP=%_HELP% _VERBOSE=%_VERBOSE% 1>&2
goto :eof

:help
echo Usage: %_BASENAME% { ^<option^> ^| ^<subcommand^> }
echo.
echo   Options:
echo     -debug      show commands executed by this script
echo     -verbose    display environment settings
echo.
echo   Subcommands:
echo     help        display this help message
goto :eof

rem output parameter(s): _GHC_HOME, _GHC_PATH
:ghc
set _GHC_HOME=
set _GHC_PATH=

set __GHC_CMD=
for /f %%f in ('where ghc.exe 2^>NUL') do set "__GHC_CMD=%%f"
if defined __GHC_CMD (
    if %_DEBUG%==1 echo %_DEBUG_LABEL% Using path of Haskell executable found in PATH 1>&2
    for %%i in ("%__GHC_CMD%") do set __GHC_BIN_DIR=%%~dpsi
    for %%f in ("!__GHC_BIN_DIR!..") do set _GHC_HOME=%%~sf
    rem keep _GHC_PATH undefined since executable already in path
    goto :eof
) else if defined HASKELL_HOME (
    set _GHC_HOME=%HASKELL_HOME%
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
set __STACK_PATH=
if exist "%_GHC_HOME%\stack\stack.exe" ( set "__STACK_PATH=;%_GHC_HOME%\stack"
) else ( echo %_WARNING_LABEL% Stack tool not installed 1>&2
)
set "_GHC_PATH=;%_GHC_HOME%\bin%__STACK_PATH%"
goto :eof

rem output parameter(s): _GIT_HOME, _GIT_PATH
:git
set _GIT_HOME=
set _GIT_PATH=

set __GIT_CMD=
for /f %%f in ('where git.exe 2^>NUL') do set "__GIT_CMD=%%f"
if defined __GIT_CMD (
    if %_DEBUG%==1 echo %_DEBUG_LABEL% Using path of Git executable found in PATH 1>&2
    rem keep _GIT_PATH undefined since executable already in path
    goto :eof
) else if defined GIT_HOME (
    set "_GIT_HOME=%GIT_HOME%"
    if %_DEBUG%==1 echo %_DEBUG_LABEL% Using environment variable GIT_HOME 1>&2
) else (
    set __PATH=C:\opt
    if exist "!__PATH!\Git\" ( set _GIT_HOME=!__PATH!\Git
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
rem path name of installation directory may contain spaces
for /f "delims=" %%f in ("%_GIT_HOME%") do set _GIT_HOME=%%~sf
if %_DEBUG%==1 echo %_DEBUG_LABEL% Using default Git installation directory %_GIT_HOME% 1>&2

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
    rem if %_DEBUG%==1 echo %_DEBUG_LABEL% where %__WHERE_ARGS%
    echo Tool paths: 1>&2
    for /f "tokens=*" %%p in ('where %__WHERE_ARGS%') do echo    %%p 1>&2
)
goto :eof

rem ##########################################################################
rem ## Cleanups

:end
endlocal & (
    if not defined HASKELL_HOME set HASKELL_HOME=%_GHC_HOME%
    set "PATH=%PATH%%_GHC_PATH%%_GIT_PATH%"
    call :print_env %_VERBOSE%
    if %_DEBUG%==1 echo %_DEBUG_LABEL% _EXITCODE=%_EXITCODE% 1>&2
    for /f "delims==" %%i in ('set ^| findstr /b "_"') do set %%i=
)
