@echo off
setlocal enabledelayedexpansion

@rem only for interactive debugging !
set _DEBUG=0

@rem #########################################################################
@rem ## Environment setup

set _EXITCODE=0

call :env
if not %_EXITCODE%==0 goto end

call :props
if not %_EXITCODE%==0 goto end

call :args %*
if not %_EXITCODE%==0 goto end

@rem #########################################################################
@rem ## Main

if %_HELP%==1 (
    call :help
    exit /b !_EXITCODE!
)
if %_CLEAN%==1 (
    call :clean
    if not !_EXITCODE!==0 goto end
)
if %_LINT%==1 (
    call :lint
    if not !_EXITCODE!==0 goto end
)
if %_COMPILE%==1 (
    call :compile
    if not !_EXITCODE!==0 goto end
)
if %_DOC%==1 (
    call :doc
    if not !_EXITCODE!==0 goto end
)
if %_RUN%==1 (
    call :run
    if not !_EXITCODE!==0 goto end
)
if %_TEST%==1 (
    call :test
    if not !_EXITCODE!==0 goto end
)
goto end

@rem #########################################################################
@rem ## Subroutine

@rem output parameters: _DEBUG_LABEL, _ERROR_LABEL, _WARNING_LABEL
@rem                    _SOURCE_DIR, _TARGET_DIR, _TARGET_DOCS_DIR
:env
set _BASENAME=%~n0
set "_ROOT_DIR=%~dp0"

call :env_colors
set _DEBUG_LABEL=%_NORMAL_BG_CYAN%[%_BASENAME%]%_RESET%
set _ERROR_LABEL=%_STRONG_FG_RED%Error%_RESET%:
set _WARNING_LABEL=%_STRONG_FG_YELLOW%Warning%_RESET%:

set "_SOURCE_DIR=%_ROOT_DIR%app"
set "_TARGET_DIR=%_ROOT_DIR%target"
set "_TARGET_GEN_DIR=%_TARGET_DIR%\gen"
set "_TARGET_DOCS_DIR=%_TARGET_DIR%\docs"

if not exist "%GHC_HOME%\bin\ghc.exe" (
    echo %_ERROR_LABEL% GHC installation not found 1>&2
    set _EXITCODE=1
    goto :eof
)
set "_GHC_CMD=%GHC_HOME%\bin\ghc.exe"
@rem option "-hidir <dir>" redirects all generated interface files into <dir>
set _GHC_OPTS=-hidir "%_TARGET_GEN_DIR%" -odir "%_TARGET_GEN_DIR%"

set "_HADDOCK_CMD=%GHC_HOME%\bin\haddock.exe"
set _HADDOCK_OPTS=--html --odir="%_TARGET_DOCS_DIR%"

set _HLINT_CMD=
if exist "%CABAL_DIR%\bin\hlint.exe" (
    set "_HLINT_CMD=%CABAL_DIR%\bin\hlint.exe"
)
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

@rem output parameters: _EXE_FILE, _GHC_OPTS, _HADDOCK_OPTS
:props
for %%f in ("%~dp0\.") do set _PACKAGE_NAME=%%~nf
set __PACKAGE_VERSION=1.0.0
set __PACKAGE_SYNOPSIS=Haskell Example
set __GHC_OPTIONS=-Wall -Werror

set __CABAL_FILE=
for /f "delims=" %%f in ('where "%_ROOT_DIR%:*.cabal" 2^>NUL') do set "__CABAL_FILE=%%f"
if exist "%__CABAL_FILE%" (
    for /f "tokens=1,* delims=:" %%i in (%__CABAL_FILE%) do (
        set __NAME=
        set __VALUE=
        for /f "delims= " %%n in ("%%i") do set __NAME=%%n
        @rem line comments start with "--"
        if defined __NAME if not "!__NAME:~0,2!"=="--" (
            @rem trim value
            for /f "tokens=*" %%v in ("%%j") do set __VALUE=%%v
            set "_!__NAME:-=_!=!__VALUE!"
        )
    )
    if defined _name set _PACKAGE_NAME=!_name!
    if defined _synopsis set __PACKAGE_SYNOPSIS=!_synopsis!
    if defined _version set __PACKAGE_VERSION=!_version!
    if defined _ghc_options set __GHC_OPTIONS=!_ghc_options!
)
set "_EXE_FILE=%_TARGET_DIR%\%_PACKAGE_NAME%.exe"
set _GHC_OPTS=%_GHC_OPTS% %__GHC_OPTIONS% -o "%_EXE_FILE%"
set _HADDOCK_OPTS=%_HADDOCK_OPTS% --title="%__PACKAGE_SYNOPSIS%" --package-name=%_PACKAGE_NAME% --package-version=%__PACKAGE_VERSION%
goto :eof

@rem input parameter: %*
@rem output parameter(s): _CLEAN, _COMPILE, _DEBUG, _RUN, _TIMER, _VERBOSE
:args
set _CLEAN=0
set _COMPILE=0
set _DOC=0
set _DOCVIEW=0
set _HELP=0
set _LINT=0
set _RUN=0
set _TEST=0
set _TIMER=0
set _VERBOSE=0
set __N=0
:args_loop
set "__ARG=%~1"
if not defined __ARG (
    if !__N!==0 set _HELP=1
    goto args_done
)
if "%__ARG:~0,1%"=="-" (
    @rem option
    if "%__ARG%"=="-debug" ( set _DEBUG=1
    ) else if "%__ARG%"=="-help" ( set _HELP=1
    ) else if "%__ARG%"=="-timer" ( set _TIMER=1
    ) else if "%__ARG%"=="-verbose" ( set _VERBOSE=1
    ) else (
        echo %_ERROR_LABEL% Unknown option %__ARG% 1>&2
        set _EXITCODE=1
        goto args_done
   )
) else (
    @rem subcommand
    if "%__ARG%"=="clean" ( set _CLEAN=1
    ) else if "%__ARG%"=="compile" ( set _COMPILE=1
    ) else if "%__ARG%"=="doc" ( set _DOC=1
    ) else if "%__ARG%"=="docview" ( set _DOC=1& set _DOCVIEW=1
    ) else if "%__ARG%"=="help" ( set _HELP=1
    ) else if "%__ARG%"=="lint" ( set _LINT=1
    ) else if "%__ARG%"=="run" ( set _COMPILE=1& set _RUN=1
    ) else if "%__ARG%"=="test" ( set _COMPILE=1& set _TEST=1
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
if %_DEBUG%==1 ( set _REDIRECT_STDOUT=1^>CON
) else ( set _REDIRECT_STDOUT=1^>NUL
)
if %_LINT%==1 if not defined _HLINT_CMD (
    echo %_WARNING_LABEL% Hlint tool not found ^(disable subcommand 'lint'^) 1>&2
    set _LINT=0
)
if %_DEBUG%==1 (
    echo %_DEBUG_LABEL% Properties : _PACKAGE_NAME=%_PACKAGE_NAME% 1>&2
    echo %_DEBUG_LABEL% Options    : _TIMER=%_TIMER% _VERBOSE=%_VERBOSE% 1>&2
    echo %_DEBUG_LABEL% Subcommands: _CLEAN=%_CLEAN% _COMPILE=%_COMPILE% _DOC=%_DOC% _LINT=%_LINT% _RUN=%_RUN% _TEST=%_TEST% 1>&2
    echo %_DEBUG_LABEL% Variables  : "GHC_HOME=%GHC_HOME%" 1>&2
    echo %_DEBUG_LABEL% Variables  : "CABAL_DIR=%CABAL_DIR%" 1>&2
)
if %_TIMER%==1 for /f "delims=" %%i in ('powershell -c "(Get-Date)"') do set _TIMER_START=%%i
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
echo     %__BEG_O%-timer%__END%      display total elapsed time
echo     %__BEG_O%-verbose%__END%    display progress messages
echo.
echo   %__BEG_P%Subcommands:%__END%
echo     %__BEG_O%clean%__END%       delete generated files
echo     %__BEG_O%compile%__END%     generate program executable
echo     %__BEG_O%doc%__END%         generate HTML documentation with %__BEG_N%Haddock%__END%
echo     %__BEG_O%help%__END%        display this help message
echo     %__BEG_O%lint%__END%        analyze Haskell source files with %__BEG_N%HLint%__END%
echo     %__BEG_O%run%__END%         execute the generated program
echo     %__BEG_O%test%__END%        execute unit tests
if %_VERBOSE%==0 goto :eof
echo.
echo   %__BEG_N%HLint%__END% hints are defined in file %__BEG_O%.hlint.yaml%__END%
goto :eof

:clean
call :rmdir "%_TARGET_DIR%"
goto :eof

@rem input parameter(s): %1=directory path
:rmdir
set "__DIR=%~1"
if not exist "%__DIR%\" goto :eof
if %_DEBUG%==1 ( echo %_DEBUG_LABEL% rmdir /s /q "%__DIR%" 1>&2
) else if %_VERBOSE%==1 ( echo Delete directory "!__DIR:%_ROOT_DIR%=!" 1>&2
)
rmdir /s /q "%__DIR%"
if not %ERRORLEVEL%==0 (
    set _EXITCODE=1
    goto :eof
)
goto :eof

:lint
if not exist "%_TARGET_DIR%" mkdir "%_TARGET_DIR%"

if %_DEBUG%==1 ( set __HLINT_OPTS=--color=auto
) else ( set __HLINT_OPTS=--color=auto "--report=%_TARGET_DIR%\report.html"
)
if %_DEBUG%==1 ( echo %_DEBUG_LABEL% "%_HLINT_CMD%" %__HLINT_OPTS% "%_SOURCE_DIR%" 1>&2
) else if %_VERBOSE%==1 ( echo Analyze Haskell source files in directory "!_SOURCE_DIR:%_ROOT_DIR%=!" 1>&2
)
call "%_HLINT_CMD%" %__HLINT_OPTS% "%_SOURCE_DIR%" %_REDIRECT_STDOUT%
if not %ERRORLEVEL%==0 (
    set _EXITCODE=1
    goto :eof
)
goto :eof

:compile
if not exist "%_TARGET_DIR%" mkdir "%_TARGET_DIR%"

call :action_required "%_EXE_FILE%" "%_SOURCE_DIR%\*.hs"
if %_ACTION_REQUIRED%==0 goto :eof

set __SOURCE_FILES=
set __N=0
for /f "usebackq delims=" %%f in (`where /r "%_SOURCE_DIR%" *.hs`) do (
    set __SOURCE_FILES=!__SOURCE_FILES! "%%f"
    set /a __N+=1
)
if %_DEBUG%==1 ( echo %_DEBUG_LABEL% "%_GHC_CMD%" %_GHC_OPTS% %__SOURCE_FILES% 1>&2
) else if %_VERBOSE%==1 ( echo Compile %__N% Haskell source files to file "!_EXE_FILE:%_ROOT_DIR%=!" 1>&2
)
call "%_GHC_CMD%" %_GHC_OPTS% %__SOURCE_FILES% %_REDIRECT_STDOUT%
if not %ERRORLEVEL%==0 (
    echo %_ERROR_LABEL% Compilation of %__N% Haskell source files failed 1>&2
    set _EXITCODE=1
    goto :eof
)
goto :eof

@rem input parameter: 1=target file 2=path (wildcards accepted)
@rem output parameter: _ACTION_REQUIRED
:action_required
set "__TARGET_FILE=%~1"
set __PATH=%~2

set __TARGET_TIMESTAMP=00000000000000
for /f "usebackq" %%i in (`powershell -c "gci -path '%__TARGET_FILE%' -ea Stop | select -last 1 -expandProperty LastWriteTime | Get-Date -uformat %%Y%%m%%d%%H%%M%%S" 2^>NUL`) do (
     set __TARGET_TIMESTAMP=%%i
)
set __SOURCE_TIMESTAMP=00000000000000
for /f "usebackq" %%i in (`powershell -c "gci -recurse -path '%__PATH%' -ea Stop | sort LastWriteTime | select -last 1 -expandProperty LastWriteTime | Get-Date -uformat %%Y%%m%%d%%H%%M%%S" 2^>NUL`) do (
    set __SOURCE_TIMESTAMP=%%i
)
call :newer %__SOURCE_TIMESTAMP% %__TARGET_TIMESTAMP%
set _ACTION_REQUIRED=%_NEWER%
if %_DEBUG%==1 (
    echo %_DEBUG_LABEL% %__TARGET_TIMESTAMP% Target : "%__TARGET_FILE%" 1>&2
    echo %_DEBUG_LABEL% %__SOURCE_TIMESTAMP% Sources: "%__PATH%" 1>&2
    echo %_DEBUG_LABEL% _ACTION_REQUIRED=%_ACTION_REQUIRED% 1>&2
) else if %_VERBOSE%==1 if %_ACTION_REQUIRED%==0 if %__SOURCE_TIMESTAMP% gtr 0 (
    echo No action required ^("!__PATH:%_ROOT_DIR%=!"^) 1>&2
)
goto :eof

@rem output parameter: _NEWER
:newer
set __TIMESTAMP1=%~1
set __TIMESTAMP2=%~2

set __DATE1=%__TIMESTAMP1:~0,8%
set __TIME1=%__TIMESTAMP1:~-6%

set __DATE2=%__TIMESTAMP2:~0,8%
set __TIME2=%__TIMESTAMP2:~-6%

if %__DATE1% gtr %__DATE2% ( set _NEWER=1
) else if %__DATE1% lss %__DATE2% ( set _NEWER=0
) else if %__TIME1% gtr %__TIME2% ( set _NEWER=1
) else ( set _NEWER=0
)
goto :eof

:doc
if not exist "%_TARGET_DOCS_DIR%" mkdir "%_TARGET_DOCS_DIR%"

set "__HTML_LIBS_DIR=%GHC_HOME%\doc\html\libraries"
if not exist "%__HTML_LIBS_DIR%" (
    echo %_ERROR_LABEL% GHC HTML documentation directory not found 1>&2
    set _EXITCODE=1
    goto :eof
)
set __HADDOCK_OPTS=%_HADDOCK_OPTS%
@rem Use "*.haddock" instead of "base.haddock" to include all interface docs.
for /f "usebackq delims=" %%f in (`where /r "%__HTML_LIBS_DIR%" base.haddock`) do (
    for %%x in (%%f) do set "__PARENT_DIR=%%~dpx"
    set __HADDOCK_OPTS=!__HADDOCK_OPTS! --read-interface=!__PARENT_DIR!,%%f
)
set __SOURCE_FILES=
for /f "usebackq delims=" %%f in (`where /r "%_SOURCE_DIR%" *.hs`) do (
    set __SOURCE_FILES=!__SOURCE_FILES! "%%f"
)
if %_DEBUG%==1 ( echo %_DEBUG_LABEL% "%_HADDOCK_CMD%" %__HADDOCK_OPTS% %__SOURCE_FILES% 1>&2
) else if %_VERBOSE%==1 ( echo Generate HTML documentation into directory "!_TARGET_DOCS_DIR:%_ROOT_DIR%=!" 1>&2
)
call "%_HADDOCK_CMD%" %__HADDOCK_OPTS% %__SOURCE_FILES%
if not %ERRORLEVEL%==0 (
    echo %_ERROR_LABEL% Generation of HTML documentation failed 1>&2
    set _EXITCODE=1
    goto :eof
)
if %_DOCVIEW%==1 if exist "%_TARGET_DOCS_DIR%\index.html" (
    if %_DEBUG%==1 ( echo %_DEBUG_LABEL% start "" "%_TARGET_DOCS_DIR%\index.html" 1>&2
	) else if %_VERBOSE%==1 ( echo Open the generated HTML pages in default browser 1>&2
	)
    start "" "%_TARGET_DOCS_DIR%\index.html"
)
goto :eof

:run
if not exist "%_EXE_FILE%" (
    echo %_ERROR_LABEL% Executable not found ^("!_EXE_FILE:%_ROOT_DIR%=!"^) 1>&2
    set _EXITCODE=1
    goto :eof
)
if %_DEBUG%==1 ( echo %_DEBUG_LABEL% "%_EXE_FILE%" 1>&2
) else if %_VERBOSE%==1 ( echo Execute Haskell program "!_EXE_FILE:%_ROOT_DIR%=!" 1>&2
)
call "%_EXE_FILE%"
if not %ERRORLEVEL%==0 (
    echo %_ERROR_LABEL% Program executable not found ^("!_EXE_FILE:%_ROOT_DIR%=!"^) 1>&2
    set _EXITCODE=1
    goto :eof
)
goto :eof

:test
echo %_WARNING_LABEL% Test not yet implemented 1>&2
goto :eof

@rem output parameter: _DURATION
:duration
set __START=%~1
set __END=%~2

for /f "delims=" %%i in ('powershell -c "$interval = New-TimeSpan -Start '%__START%' -End '%__END%'; Write-Host $interval"') do set _DURATION=%%i
goto :eof

@rem #########################################################################
@rem ## Cleanups

:end
if %_TIMER%==1 (
    for /f "delims=" %%i in ('powershell -c "(Get-Date)"') do set __TIMER_END=%%i
    call :duration "%_TIMER_START%" "!__TIMER_END!"
    echo Total elapsed time: !_DURATION! 1>&2
)
if %_DEBUG%==1 echo %_DEBUG_LABEL% _EXITCODE=%_EXITCODE% 1>&2
exit /b %_EXITCODE%
endlocal
