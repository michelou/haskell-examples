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

set _GIT_PATH=
set _MAVEN_PATH=
set _STACK_PATH=

@rem lts -> 8, latest -> 9
call :ghc 8
if not %_EXITCODE%==0 goto end

call :stack
if not %_EXITCODE%==0 goto end

call :git
if not %_EXITCODE%==0 goto end

@rem %1=vendor, %2=version
@rem eg. bellsoft, corretto, bellsoft, openj9, opendjdk, redhat, sapmachine, zulu
call :jdk "openjdk" 11
if not %_EXITCODE%==0 goto end

call :maven
if not %_EXITCODE%==0 goto end

goto end

@rem #########################################################################
@rem ## Subroutines

@rem output parameters: _DEBUG_LABEL, _ERROR_LABEL, _WARNING_LABEL
:env
set _BASENAME=%~n0
set _DRIVE_NAME=H
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
    if "%__ARG%"=="-debug" ( set _DEBUG=1
    ) else if "%__ARG%"=="-verbose" ( set _VERBOSE=1
    ) else (
        echo %_ERROR_LABEL% Unknown option %__ARG% 1>&2
        set _EXITCODE=1
        goto args_done
    )
) else (
    @rem subcommand
    if "%__ARG%"=="help" ( set _HELP=1
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
call :subst %_DRIVE_NAME% "%_ROOT_DIR%"
if not %_EXITCODE%==0 goto :eof
if %_DEBUG%==1 (
    call :env_uptodate
    if not !_EXITCODE!==0 goto end
)
if %_DEBUG%==1 (
    echo %_DEBUG_LABEL% Options  : _HELP=%_HELP% _VERBOSE=%_VERBOSE% 1>&2
    echo %_DEBUG_LABEL% Variables: _DRIVE_NAME=%_DRIVE_NAME% 1>&2
)
goto :eof

@rem input parameter(s): %1: drive letter, %2: path to be substituted
:subst
set __DRIVE_NAME=%~1
set "__GIVEN_PATH=%~2"

if not "%__DRIVE_NAME:~-1%"==":" set __DRIVE_NAME=%__DRIVE_NAME%:
if /i "%__DRIVE_NAME%"=="%__GIVEN_PATH:~0,2%" goto :eof

if "%__GIVEN_PATH:~-1%"=="\" set "__GIVEN_PATH=%__GIVEN_PATH:~0,-1%"
if not exist "%__GIVEN_PATH%" (
    echo %_ERROR_LABEL% Provided path does not exist ^(%__GIVEN_PATH%^) 1>&2
    set _EXITCODE=1
    goto :eof
)
for /f "tokens=1,2,*" %%f in ('subst ^| findstr /b "%__DRIVE_NAME%" 2^>NUL') do (
    set "__SUBST_PATH=%%h"
    if "!__SUBST_PATH!"=="!__GIVEN_PATH!" (
        set __MESSAGE=
        for /f %%i in ('subst ^| findstr /b "%__DRIVE_NAME%\"') do "set __MESSAGE=%%i"
        if defined __MESSAGE (
            if %_DEBUG%==1 ( echo %_DEBUG_LABEL% !__MESSAGE! 1>&2
            ) else if %_VERBOSE%==1 ( echo !__MESSAGE! 1>&2
            )
        )
        goto :eof
    )
)
if %_DEBUG%==1 ( echo %_DEBUG_LABEL% subst "%__DRIVE_NAME%" "%__GIVEN_PATH%" 1>&2
) else if %_VERBOSE%==1 ( echo Assign path %__GIVEN_PATH% to drive %__DRIVE_NAME% 1>&2
)
subst "%__DRIVE_NAME%" "%__GIVEN_PATH%"
if not %ERRORLEVEL%==0 (
    echo %_ERROR_LABEL% Failed to assigned drive %__DRIVE_NAME% to path 1>&2
    set _EXITCODE=1
    goto :eof
)
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

@rem input parameter: %1=GHC major version
@rem output parameter(s): _CABAL_DIR, _GHC_HOME
:ghc
set __GHC_VERSION=%~1
if not defined __GHC_VERSION set __GHC_VERSION=8

set _CABAL_DIR=
set _GHC_HOME=

set __GHC_CMD=
for /f %%f in ('where ghc.exe 2^>NUL') do set "__GHC_CMD=%%f"
if defined __GHC_CMD (
    if %_DEBUG%==1 echo %_DEBUG_LABEL% Using path of Haskell executable found in PATH 1>&2
    for %%i in ("%__GHC_CMD%") do set "__GHC_BIN_DIR=%%~dpi"
    for %%f in ("!__GHC_BIN_DIR!\.") do set "_GHC_HOME=%%~dpf"
    goto :eof
) else if defined HASKELL_HOME (
    set "_GHC_HOME=%HASKELL_HOME%"
    if %_DEBUG%==1 echo %_DEBUG_LABEL% Using environment variable HASKELL_HOME 1>&2
) else (
    set __PATH=C:\opt
    for /f %%f in ('dir /ad /b "!__PATH!\ghc-%__GHC_VERSION%*" 2^>NUL') do set "_GHC_HOME=!__PATH!\%%f"
    if not defined _GHC_HOME (
        set "__PATH=%ProgramFiles%"
        for /f "delims=" %%f in ('dir /ad /b "!__PATH!\ghc-%__GHC_VERSION%*" 2^>NUL') do set "_GHC_HOME=!__PATH!\%%f"
    )
)
if not exist "%_GHC_HOME%\bin\ghc.exe" (
    echo %_ERROR_LABEL% Executable ghc.exe not found ^(%_GHC_HOME%^) 1>&2
    set _EXITCODE=1
    goto :eof
)
set "_CABAL_DIR=%APPDATA%\cabal"
if not exist "%_GHC_HOME%\bin\cabal.exe" (
    echo %_WARNING_LABEL% Cabal executable not installed 1>&2
)
if not exist "%_CABAL_DIR%\bin\hlint.exe" (
    echo %_WARNING_LABEL% HLint tool not installed 1>&2
)
if not exist "%_CABAL_DIR%\bin\hpack.exe" (
    echo %_WARNING_LABEL% HPack tool not installed 1>&2
)
if not exist "%_CABAL_DIR%\bin\htfpp.exe" (
    echo %_WARNING_LABEL% HTF tool not installed 1>&2
)
set _ORMOLU_HOME=
for /f %%f in ('dir /b "%_GHC_HOME%\ormolu-*" 2^>NUL') do (
    set "_ORMOLU_HOME=%_GHC_HOME%\%%f"
)
if not defined _ORMOLU_HOME echo %_WARNING_LABEL% ormolu tool not installed 1>&2
goto :eof

@rem output parameter(s): _STACK_HOME, _STACK_PATH
:stack
set _STACK_HOME=
set _STACK_PATH=

set __STACK_CMD=
for /f %%f in ('where stack.exe 2^>NUL') do set "__STACK_CMD=%%f"
if defined __GIT_CMD (
    for %%i in (%__GIT_CMD%) do set "_STACK_HOME=%%~dpi"
    if %_DEBUG%==1 echo %_DEBUG_LABEL% Using path of Stack executable found in PATH 1>&2
    goto :eof
) else if defined STACK_HOME (
    set "_STACK_HOME=%STACK_HOME%"
    if %_DEBUG%==1 echo %_DEBUG_LABEL% Using environment variable STACK_HOME 1>&2
) else (
    set __PATH=C:\opt
    for /f %%f in ('dir /ad /b "!__PATH!\stack-2*" 2^>NUL') do set "_STACK_HOME=!__PATH!\%%f"
    if not defined _STACK_HOME (
        set "__PATH=%ProgramFiles%"
        for /f %%f in ('dir /ad /b "!__PATH!\stack-2*" 2^>NUL') do set "_STACK_HOME=!__PATH!\%%f"
    )
)
if not exist "%_STACK_HOME%\stack.exe" (
    echo %_ERROR_LABEL% Stack executable not found ^(%_STACK_HOME%^) 1>&2
    set _EXITCODE=1
    goto :eof
)
set "_STACK_PATH=;%_STACK_HOME%\bin"
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

@rem input parameter: %1=vendor %1^=required version
@rem output parameter(s): _JDK_HOME
@rem Note: JAVA_HOME is required for Maven
:jdk
set _JDK_HOME=

set __VENDOR=%~1
set __VERSION=%~2
if not defined __VENDOR ( set __JDK_NAME=jdk-%__VERSION%
) else ( set __JDK_NAME=jdk-%__VENDOR%-%__VERSION%
)
set __JAVAC_CMD=
for /f %%f in ('where javac.exe 2^>NUL') do set "__JAVAC_CMD=%%f"
if defined __JAVAC_CMD (
    call :jdk_version "%__JAVAC_CMD%"
    if !_JDK_VERSION!==%__VERSION% (
        for %%i in ("%__JAVAC_CMD%") do set "__BIN_DIR=%%~dpi"
        for %%f in ("%__BIN_DIR%") do set "_JDK_HOME=%%~dpf"
    ) else (
        echo %_ERROR_LABEL% Required JDK installation not found ^(%__JDK_NAME%^) 1>&2
        set _EXITCODE=1
        goto :eof
    )
)
if defined JDK_HOME (
    set "_JDK_HOME=%JDK_HOME%"
    if %_DEBUG%==1 echo %_DEBUG_LABEL% Using environment variable JDK_HOME 1>&2
) else (
    set _PATH=C:\opt
    for /f "delims=" %%f in ('dir /ad /b "!_PATH!\%__JDK_NAME%*" 2^>NUL') do set "_JDK_HOME=!_PATH!\%%f"
    if not defined _JDK_HOME (
        set "_PATH=%ProgramFiles%\Java"
        for /f %%f in ('dir /ad /b "!_PATH!\%__JDK_NAME%*" 2^>NUL') do set "_JDK_HOME=!_PATH!\%%f"
    )
    if defined _JDK_HOME (
        if %_DEBUG%==1 echo %_DEBUG_LABEL% Using default Java SDK installation directory !_JDK_HOME! 1>&2
    )
)
if not exist "%_JDK_HOME%\bin\javac.exe" (
    echo %_ERROR_LABEL% Executable javac.exe not found ^(%_JDK_HOME%^) 1>&2
    set _EXITCODE=1
    goto :eof
)
goto :eof

@rem input parameter(s): %1=javac file path
@rem output parameter(s): _JDK_VERSION
:jdk_version
set "__JAVAC_CMD=%~1"
if not exist "%__JAVAC_CMD%" (
    echo %_ERROR_LABEL% Command javac.exe not found ^("%__JAVAC_CMD%"^) 1>&2
    set _EXITCODE=1
    goto :eof
)
set __JAVAC_VERSION=
for /f "usebackq tokens=1,*" %%i in (`"%__JAVAC_CMD%" -version 2^>^&1`) do set __JAVAC_VERSION=%%j
if "!__JAVAC_VERSION:~0,2!"=="17" ( set _JDK_VERSION=17
) else if "!__JAVAC_VERSION:~0,2!"=="14" ( set _JDK_VERSION=14
) else if "!__JAVAC_VERSION:~0,2!"=="11" ( set _JDK_VERSION=11
) else if "!__JAVAC_VERSION:~0,3!"=="1.8" ( set _JDK_VERSION=8
) else if "!__JAVAC_VERSION:~0,3!"=="1.7" ( set _JDK_VERSION=7
) else (
    set _JDK_VERSION=
    echo %_ERROR_LABEL% Unsupported JDK version %__JAVAC_VERSION% 1>&2
    set _EXITCODE=1
    goto :eof
)
goto :eof

@rem output parameters: _MAVEN_HOME, _MAVEN_PATH
:maven
set _MAVEN_HOME=
set _MAVEN_PATH=

set __MAVEN_CMD=
for /f %%f in ('where mvn.cmd 2^>NUL') do set "__MAVEN_CMD=%%f"
if defined __MAVEN_CMD (
    for %%i in ("%__MAVEN_CMD%") do set "__MAVEN_BIN_DIR=%%~dpi"
    for %%f in ("!__MAVEN_BIN_DIR!\.") do set "_MAVEN_HOME=%%~dpf"
    if %_DEBUG%==1 echo %_DEBUG_LABEL% Using path of Maven executable found in PATH 1>&2
    goto :eof
) else if defined MAVEN_HOME (
    set "_MAVEN_HOME=%MAVEN_HOME%"
    if %_DEBUG%==1 echo %_DEBUG_LABEL% Using environment variable MAVEN_HOME 1>&2
) else (
    set __PATH=C:\opt
    for /f %%f in ('dir /ad /b "!__PATH!\apache-maven-*" 2^>NUL') do set "_MAVEN_HOME=!__PATH!\%%f"
    if defined _MAVEN_HOME (
        if %_DEBUG%==1 echo %_DEBUG_LABEL% Using default Maven installation directory !_MAVEN_HOME! 1>&2
    )
)
if not exist "%_MAVEN_HOME%\bin\mvn.cmd" (
    echo %_ERROR_LABEL% Maven executable not found ^(%_MAVEN_HOME%^) 1>&2
    set _EXITCODE=1
    goto :eof
)
set "_MAVEN_PATH=;%_MAVEN_HOME%\bin"
goto :eof

:print_env
set __VERBOSE=%1
set "__VERSIONS_LINE1=  "
set "__VERSIONS_LINE2=  "
set "__VERSIONS_LINE3=  "
set __WHERE_ARGS=
where /q "%GHC_HOME%\bin:cabal.exe"
if %ERRORLEVEL%==0 (
    for /f "tokens=1,2,*" %%i in ('"%GHC_HOME%\bin\cabal.exe" --version ^| findstr install') do set "__VERSIONS_LINE1=%__VERSIONS_LINE1% cabal %%~k,"
    set __WHERE_ARGS=%__WHERE_ARGS% "%GHC_HOME%\bin:cabal.exe"
)
where /q "%GHC_HOME%\bin:ghc.exe"
if %ERRORLEVEL%==0 (
    for /f "delims=, tokens=1,*" %%i in ('"%GHC_HOME%\bin\ghc.exe" --version') do set "__VERSIONS_LINE1=%__VERSIONS_LINE1% ghc%%~j,"
    set __WHERE_ARGS=%__WHERE_ARGS% "%GHC_HOME%\bin:ghc.exe"
)
where /q "%STACK_HOME%:stack.exe"
if %ERRORLEVEL%==0 (
    for /f "tokens=1,2,*" %%i in ('"%STACK_HOME%\stack.exe" --version') do set "__VERSIONS_LINE1=%__VERSIONS_LINE1% stack %%~j"
    set __WHERE_ARGS=%__WHERE_ARGS% "%STACK_HOME%:stack.exe"
)
where /q "%GHC_HOME%\bin:haddock.exe"
if %ERRORLEVEL%==0 (
    for /f "tokens=1,2,3,*" %%i in ('"%GHC_HOME%\bin\haddock.exe" --version ^| findstr version') do set "__VERSIONS_LINE1=%__VERSIONS_LINE1% haddock %%~k"
    set __WHERE_ARGS=%__WHERE_ARGS% "%GHC_HOME%\bin:haddock.exe"
)
where /q "%CABAL_DIR%\bin:hlint.exe"
if %ERRORLEVEL%==0 (
    for /f "tokens=1,2,*" %%i in ('"%CABAL_DIR%\bin\hlint.exe" --version') do set "__VERSIONS_LINE2=%__VERSIONS_LINE2% hlint %%~j"
    set __WHERE_ARGS=%__WHERE_ARGS% "%CABAL_DIR%\bin:hlint.exe"
)
where /q "%CABAL_DIR%\bin:hpack.exe"
if %ERRORLEVEL%==0 (
    for /f "tokens=1,2,*" %%i in ('"%CABAL_DIR%\bin\hpack.exe" --version') do set "__VERSIONS_LINE2=%__VERSIONS_LINE2% hpack %%~k,"
    set __WHERE_ARGS=%__WHERE_ARGS% "%CABAL_DIR%\bin:hpack.exe"
)
where /q "%CABAL_DIR%\bin:htfpp.exe"
if %ERRORLEVEL%==0 (
    for /f "tokens=*" %%i in ('"%CABAL_DIR%\bin\htfpp.exe" --version 2^>^&1') do set "__VERSIONS_LINE2=%__VERSIONS_LINE2% htfpp %%~i,"
    set __WHERE_ARGS=%__WHERE_ARGS% "%CABAL_DIR%\bin:htfpp.exe"
)
where /q "%ORMOLU_HOME%\bin:ormolu.exe"
if %ERRORLEVEL%==0 (
    for /f "tokens=1,2,*" %%i in ('"%ORMOLU_HOME%\bin\ormolu.exe" --version ^| findstr /b ormolu') do set "__VERSIONS_LINE2=%__VERSIONS_LINE2% ormolu %%~j"
    set __WHERE_ARGS=%__WHERE_ARGS% "%ORMOLU_HOME%\bin:ormolu.exe"
)
where /q "%JAVA_HOME%\bin:java.exe"
if %ERRORLEVEL%==0 (
    for /f "tokens=1,2,3,*" %%i in ('"%JAVA_HOME%\bin\java.exe" -version 2^>^&1 ^| findstr version') do set "__VERSIONS_LINE3=%__VERSIONS_LINE3% java %%~k,"
    set __WHERE_ARGS=%__WHERE_ARGS% "%JAVA_HOME%\bin:java.exe"
)
where /q "%MAVEN_HOME%\bin:mvn.cmd"
if %ERRORLEVEL%==0 (
    for /f "tokens=1,2,3,*" %%i in ('"%MAVEN_HOME%\bin\mvn.cmd" -version 2^>^&1 ^| findstr /b Apache') do set "__VERSIONS_LINE3=%__VERSIONS_LINE3% mvn %%~k,"
    set __WHERE_ARGS=%__WHERE_ARGS% "%MAVEN_HOME%\bin:mvn.cmd"
)
where /q "%GIT_HOME%\bin:git.exe"
if %ERRORLEVEL%==0 (
    for /f "tokens=1,2,*" %%i in ('"%GIT_HOME%\bin\git.exe" --version') do set "__VERSIONS_LINE3=%__VERSIONS_LINE3% git %%k,"
    set __WHERE_ARGS=%__WHERE_ARGS% "%GIT_HOME%\bin:git.exe"
)
where /q "%GIT_HOME%\usr\bin:diff.exe"
if %ERRORLEVEL%==0 (
    for /f "tokens=1-3,*" %%i in ('"%GIT_HOME%\usr\bin\diff.exe" --version ^| findstr /B diff') do set "__VERSIONS_LINE3=%__VERSIONS_LINE3% diff %%l"
    set __WHERE_ARGS=%__WHERE_ARGS% "%GIT_HOME%\usr\bin:diff.exe"
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
    if defined CABAL_DIR echo    "CABAL_DIR=%CABAL_DIR%" 1>&2
    if defined GHC_HOME echo    "GHC_HOME=%GHC_HOME%" 1>&2
    if defined GIT_HOME echo    "GIT_HOME=%GIT_HOME%" 1>&2
    if defined JAVA_HOME echo    "JAVA_HOME=%JAVA_HOME%" 1>&2
    if defined MAVEN_HOME echo    "MAVEN_HOME=%MAVEN_HOME%" 1>&2
    if defined ORMOLU_HOME echo    "ORMOLU_HOME=%ORMOLU_HOME%" 1>&2
    if defined STACK_HOME echo    "STACK_HOME=%STACK_HOME%" 1>&2
)
goto :eof

@rem #########################################################################
@rem ## Cleanups

:end
endlocal & (
    if not defined CABAL_DIR set "CABAL_DIR=%_CABAL_DIR%"
    if not defined GHC_HOME set "GHC_HOME=%_GHC_HOME%"
    if not defined GIT_HOME set "GIT_HOME=%_GIT_HOME%"
    @rem Variable JAVA_HOME must be defined for Maven
    if not defined JAVA_HOME set "JAVA_HOME=%_JDK_HOME%"
    if not defined MAVEN_HOME set "MAVEN_HOME=%_MAVEN_HOME%"
    if not defined ORMOLU_HOME set "ORMOLU_HOME=%_ORMOLU_HOME%"
    if not defined STACK_HOME set "STACK_HOME=%_STACK_HOME%"
    for /f %%i in ('stack.exe --version 2^>NUL') do set STACK_WORK=target
    set "PATH=%PATH%;%_GHC_HOME%\bin;%_CABAL_DIR%\bin;%_STACK_HOME%;%_GIT_PATH%%_MAVEN_PATH%"
    call :print_env %_VERBOSE%
    if not "%CD:~0,2%"=="%_DRIVE_NAME%:" (
        if %_DEBUG%==1 echo %_DEBUG_LABEL% cd /d %_DRIVE_NAME%: 1>&2
        cd /d %_DRIVE_NAME%:
    )
    if %_DEBUG%==1 echo %_DEBUG_LABEL% _EXITCODE=%_EXITCODE% 1>&2
    for /f "delims==" %%i in ('set ^| findstr /b "_"') do set %%i=
)
