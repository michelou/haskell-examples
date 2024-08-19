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
goto end

@rem #########################################################################
@rem ## Subroutine

@rem output parameters: _DEBUG_LABEL, _ERROR_LABEL, _WARNING_LABEL
@rem                    _SOURCE_DIR, _TARGET_DIR, _TARGET_GEN_DIR
:env
set _BASENAME=%~n0
set "_ROOT_DIR=%~dp0"

call :env_colors
set _DEBUG_LABEL=%_NORMAL_BG_CYAN%[%_BASENAME%]%_RESET%
set _ERROR_LABEL=%_STRONG_FG_RED%Error%_RESET%:
set _WARNING_LABEL=%_STRONG_FG_YELLOW%Warning%_RESET%:

set "_SOURCE_DIR=%_ROOT_DIR%src"
set "_LIB_DIR=%_ROOT_DIR%lib"
set "_TARGET_DIR=%_ROOT_DIR%target"
set "_TARGET_GEN_DIR=%_TARGET_DIR%\gen"
set "_TARGET_DOCS_DIR=%_TARGET_DIR%\docs"

if not exist "%GHC_HOME%\bin\ghc.exe" (
    echo %_ERROR_LABEL% GHC executable not found 1>&2
    set _EXITCODE=1
    goto :eof
)
set "_GHC_CMD=%GHC_HOME%\bin\ghc.exe"
@rem https://mpickering.github.io/ghc-docs/build-html/users_guide/using-warnings.html
set _GHC_WARNINGS=-Wall -Wincomplete-uni-patterns
@rem option "-hidir <dir>" redirects all generated interface files into <dir>
set _GHC_OPTS=%_GHC_WARNINGS% -hidir "%_TARGET_GEN_DIR%" -odir "%_TARGET_GEN_DIR%"

if not exist "%GHC_HOME%\bin\haddock.exe" (
    echo %_ERROR_LABEL% Haddock executable not found 1>&2
    set _EXITCODE=1
    goto :eof
)
set "_HADDOCK_CMD=%GHC_HOME%\bin\haddock.exe"
set _HADDOCK_OPTS=--html --odir="%_TARGET_DOCS_DIR%"

set _TAR_CMD=tar.exe
set _TAR_OPTS=

@rem we use the newer PowerShell version if available
where /q pwsh.exe
if %ERRORLEVEL%==0 ( set _PWSH_CMD=pwsh.exe
) else ( set _PWSH_CMD=powershell.exe
)
goto :eof

:env_colors
@rem ANSI colors in standard Windows 10 shell
@rem see https://gist.github.com/mlocati/#file-win10colors-cmd

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

@rem we define _RESET in last position to avoid crazy console output with type command
set _BOLD=[1m
set _UNDERSCORE=[4m
set _INVERSE=[7m
set _RESET=[0m
goto :eof

@rem output parameters: _EXEC_DEFAULT, _EXECUTABLE_N, _HADDOCK_OPTS
:props
set _EXECUTABLE_N=0

set __PACKAGE_NAME=Colors
set __PACKAGE_VERSION=0.0.1
set __PACKAGE_SYNOPSIS=Haskell Example

set __EXECUTABLE_NAME=
for /f "delims=" %%f in ('dir /b "%_ROOT_DIR%" *.cabal') do set "__CABAL_FILE=%%f"
if exist "%__CABAL_FILE%" (
    for /f "tokens=1,* delims=:" %%i in (%__CABAL_FILE%) do (
        for /f "delims= " %%n in ("%%i") do set __NAME=%%~n
        @rem line comments start with "--"
        if "!__NAME:~0,2!"=="--" (
            @rem skip line comments
        ) else if "!__NAME!"=="executable" (
            set /a _EXECUTABLE_N+=1
            set "__EXECUTABLE_NAME=%%i"
            set "__EXECUTABLE_NAME=!__EXECUTABLE_NAME: =!"
            set _EXECUTABLE[!_EXECUTABLE_N!][executable]=!__EXECUTABLE_NAME:executable=!
            set _EXECUTABLE[!_EXECUTABLE_N!][args]=1
        ) else (
            @rem trim value
            for /f "tokens=*" %%v in ("%%j") do set __VALUE=%%v
            if not defined __EXECUTABLE_NAME (
                set "_!__NAME:-=_!=!__VALUE!"
            ) else if "!__NAME!"=="main-is" (
                set _EXECUTABLE[!_EXECUTABLE_N!][!__NAME!]=!__VALUE:/=\!
            ) else if "!__NAME!"=="default-language" (
                set _EXECUTABLE[!_EXECUTABLE_N!][!__NAME!]=!__VALUE!
            ) else if "!__NAME!"=="ghc-options" (
                set _EXECUTABLE[!_EXECUTABLE_N!][!__NAME!]=!__VALUE!
            )
        )
    )
    if defined _name set __PACKAGE_NAME=!_name!
    if defined _synopsis set __PACKAGE_SYNOPSIS=!_synopsis!
    if defined _version set __PACKAGE_VERSION=!_version!
) else (
    @rem we run a selection of three code examples
    set _EXECUTABLE[1][executable]=rpar
    set _EXECUTABLE[1][main-is]=src/rpar.hs
    set _EXECUTABLE[1][ghc-options]=-threaded
    set _EXECUTABLE[1][default-language]=Haskell2010

    set _EXECUTABLE[2][executable]=strat
    set _EXECUTABLE[2][main-is]=src/strat.hs
    set _EXECUTABLE[2][ghc-options]=-threaded
    set _EXECUTABLE[2][default-language]=Haskell2010

    set _EXECUTABLE[3][executable]=parmonad
    set _EXECUTABLE[3][main-is]=src/parmonad.hs
    set _EXECUTABLE[3][ghc-options]=-threaded
    set _EXECUTABLE[3][default-language]=Haskell2010

    set _EXECUTABLE_N=3
)
set _EXEC_DEFAULT=rpar
@rem set __N=0
@rem for /l %%i in (1, 1, %_EXECUTABLE_N%) do (
@rem     echo %%i executable      =!_EXECUTABLE[%%i][executable]!
@rem     echo %%i main-is         =!_EXECUTABLE[%%i][main-is]!
@rem     echo %%i ghc-options     =!_EXECUTABLE[%%i][ghc-options]!
@rem     echo %%i default-language=!_EXECUTABLE[%%i][default-language]!
@rem     echo.
@rem )
@rem set "_EXE_FILE=%_TARGET_DIR%\%__PACKAGE_NAME%.exe"
set _HADDOCK_OPTS=%_HADDOCK_OPTS% --title="%__PACKAGE_SYNOPSIS:"='%" --package-name=%__PACKAGE_NAME% --package-version=%__PACKAGE_VERSION%
goto :eof

@rem input parameter: %*
@rem output parameters: _CLEAN, _COMPILE, _DEBUG, _RUN, _TIMER, _VERBOSE
:args
set _CLEAN=0
set _COMPILE=0
set _DOC=0
set _HELP=0
set _EXEC=%_EXEC_DEFAULT%
set _LINT=0
set _RUN=0
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
    ) else if "%__ARG:~0,6%"=="-exec:" (
        call :set_exec "!__ARG:~6!"
        if not !_EXITCODE!== 0 goto args_done
    ) else if "%__ARG%"=="-timer" ( set _TIMER=1
    ) else if "%__ARG%"=="-verbose" ( set _VERBOSE=1
    ) else (
        echo %_ERROR_LABEL% Unknown option "%__ARG%" 1>&2
        set _EXITCODE=1
        goto args_done
   )
) else (
    @rem subcommand
    if "%__ARG%"=="clean" ( set _CLEAN=1
    ) else if "%__ARG%"=="compile" ( set _COMPILE=1
    ) else if "%__ARG%"=="doc" ( set _DOC=1
    ) else if "%__ARG%"=="help" ( set _HELP=1
    ) else if "%__ARG%"=="lint" ( set _LINT=1
    ) else if "%__ARG%"=="run" ( set _COMPILE=1& set _RUN=1
    ) else (
        echo %_ERROR_LABEL% Unknown subcommand "%__ARG%" 1>&2
        set _EXITCODE=1
        goto args_done
    )
    set /a __N+=1
)
shift
goto args_loop
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
    echo %_DEBUG_LABEL% Subcommands: _CLEAN=%_CLEAN% _COMPILE=%_COMPILE% _DOC=%_DOC% _LINT=%_LINT% _RUN=%_RUN% 1>&2
    echo %_DEBUG_LABEL% Variables  : "CABAL_DIR=%CABAL_DIR%" 1>&2
    echo %_DEBUG_LABEL% Variables  : "GHC_HOME=%GHC_HOME%" 1>&2
    echo %_DEBUG_LABEL% Variables  : _EXEC=%_EXEC% 1>&2
)
if %_TIMER%==1 for /f "delims=" %%i in ('call %"_PWSH_CMD%" -c "(Get-Date)"') do set _TIMER_START=%%i
goto :eof

:help
if %_VERBOSE%==1 (
    set __BEG_P=%_STRONG_FG_CYAN%
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
echo     %__BEG_O%-debug%__END%      print commands executed by this script
echo     %__BEG_O%-exec:^<exec^>%__END%  define Cabal executable ^(default: %__BEG_O%%_EXEC_DEFAULT%%__END%^)
echo     %__BEG_O%-timer%__END%      print total execution time
echo     %__BEG_O%-verbose%__END%    print progress messages
echo.
echo   %__BEG_P%Subcommands:%__END%
echo     %__BEG_O%clean%__END%       delete generated files
echo     %__BEG_O%compile%__END%     generate program executable
echo     %__BEG_O%doc%__END%         generate HTML documentation with %__BEG_N%Haddock%__END%
echo     %__BEG_O%help%__END%        print this help message
echo     %__BEG_O%lint%__END%        analyze Haskell source files with %__BEG_N%HLint%__END%
echo     %__BEG_O%run%__END%         execute the generated program "%__BEG_O%!_EXE_FILE:%_ROOT_DIR%=!%__END%"
goto :eof

@rem output parameter: _EXEC
:set_exec
set __ARG=%~1
set __VALID=0
for /f %%i in ('powershell -C "$s='%__ARG%'; if($s -match '^[\w$]+(\.[\w$]+)*$'){1}else{0}"') do set __VALID=%%i
@rem if %_DEBUG%==1 echo %_DEBUG_LABEL% __ARG=%__ARG% __VALID=%__VALID% 1>&2
if %__VALID%==0 (
    echo %_ERROR_LABEL% Invalid name passed to option "-main" ^(%__ARG%^) 1>&2
    set _EXITCODE=1
    goto :eof
)
set _EXEC=%__ARG%
goto :eof

:clean
call :rmdir "%_TARGET_DIR%"
call :rmdir "%_ROOT_DIR%dist-newstyle"
goto :eof

@rem input parameter: %1=directory path
:rmdir
set "__DIR=%~1"
if not exist "%__DIR%\" goto :eof
if %_DEBUG%==1 ( echo %_DEBUG_LABEL% rmdir /s /q "%__DIR%" 1>&2
) else if %_VERBOSE%==1 ( echo Delete directory "!__DIR:%_ROOT_DIR%=!" 1>&2
)
rmdir /s /q "%__DIR%"
if not %ERRORLEVEL%==0 (
    echo %_ERROR_LABEL% Failed to delete directory "!__DIR:%_ROOT_DIR%=!" 1>&2
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
    echo %_ERROR_LABEL% Failed to analyze Haskell source files in directory "!_SOURCE_DIR:%_ROOT_DIR%=!" 1>&2
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
if defined _EXEC (
    for /l %%i in (1, 1, %_EXECUTABLE_N%) do (
        if "!_EXECUTABLE[%%i][executable]!"=="%_EXEC%" (
            set "__SOURCE_FILES=%_ROOT_DIR%!_EXECUTABLE[%%i][main-is]!"
            set __N=%%i
        )
    )
)
if not defined __SOURCE_FILES (
    echo %_WARNING_LABEL% Executable %_EXEC% not found 1>&2
    goto :eof
)
call :action_required "%_TARGET_DIR%\%_EXEC%.exe" "%_SOURCE_DIR%\*.hs"
if %_ACTION_REQUIRED%==0 goto :eof

call :includes
if not %_EXITCODE%==0 goto :eof

set __GHC_OPTS=%_GHC_OPTS% !_EXECUTABLE[%__N%][ghc-options]! -i"%_INCLUDES%" -X!_EXECUTABLE[%__N%][default-language]! -o "%_TARGET_DIR%\%_EXEC%.exe"
@rem TODO:  -optP-include -optPdist/build/autogen/cabal_macros.h
if %_DEBUG%==1 set __GHC_OPTS=%__GHC_OPTS% -Rghc-timing

if %_DEBUG%==1 ( echo %_DEBUG_LABEL% "%_GHC_CMD%" %__GHC_OPTS% %__SOURCE_FILES% 1>&2
) else if %_VERBOSE%==1 ( echo Compile Haskell source files to directory "!_TARGET_DIR:%_ROOT_DIR%=!" 1>&2
)
call "%_GHC_CMD%" %__GHC_OPTS% %__SOURCE_FILES% %_REDIRECT_STDOUT%
if not %ERRORLEVEL%==0 (
    echo %_ERROR_LABEL% Failed to compile Haskell source files to directory "!_TARGET_DIR:%_ROOT_DIR%=!" 1>&2
    set _EXITCODE=1
    goto :eof
)
goto :eof

@rem input parameter: 1=target file 2=path (wildcards accepted)
@rem output parameter: _ACTION_REQUIRED
:action_required
set "__TARGET_FILE=%~1"
set "__PATH=%~2"

set __TARGET_TIMESTAMP=00000000000000
for /f "usebackq" %%i in (`call %"_PWSH_CMD%" -c "gci -path '%__TARGET_FILE%' -ea Stop | select -last 1 -expandProperty LastWriteTime | Get-Date -uformat %%Y%%m%%d%%H%%M%%S" 2^>NUL`) do (
     set __TARGET_TIMESTAMP=%%i
)
set __SOURCE_TIMESTAMP=00000000000000
for /f "usebackq" %%i in (`call %"_PWSH_CMD%" -c "gci -recurse -path '%__PATH%' -ea Stop | sort LastWriteTime | select -last 1 -expandProperty LastWriteTime | Get-Date -uformat %%Y%%m%%d%%H%%M%%S" 2^>NUL`) do (
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

@rem output parameter: _INCLUDES
:includes
if not exist "%_LIB_DIR%\" mkdir "%_LIB_DIR%"

@rem http://hackage.haskell.org/package/monad-par
set __PACKAGE_NAME=monad-par-0.3.6
call :install_package "%__PACKAGE_NAME%" "%_LIB_DIR%"
if not %_EXITCODE%==0 goto :eof

set "_INCLUDES=%_LIB_DIR%\%__PACKAGE_NAME%"

@rem http://hackage.haskell.org/package/parallel
set __PACKAGE_NAME=parallel-3.2.2.0
call :install_package "%__PACKAGE_NAME%" "%_LIB_DIR%"
if not %_EXITCODE%==0 goto :eof

set "_INCLUDES=%_INCLUDES%:%_LIB_DIR%\%__PACKAGE_NAME%"

@rem http://hackage.haskell.org/package/timeit
set __PACKAGE_NAME=timeit-2.0
call :install_package "%__PACKAGE_NAME%" "%_LIB_DIR%"
if not %_EXITCODE%==0 goto :eof

set "_INCLUDES=%_INCLUDES%:%_LIB_DIR%\%__PACKAGE_NAME%"
goto :eof

@rem input parameters: %1=package name, %2=installation directory path
:install_package
set "__PACKAGE_NAME=%~1"
set "__INSTALL_DIR=%~2"

if exist "%__INSTALL_DIR%\%__PACKAGE_NAME%" goto :eof

set "__TEMP_DIR=%TEMP%\lib"
if not exist "%__TEMP_DIR%" mkdir "%__TEMP_DIR%"

set __PACKAGE_URL=https://hackage.haskell.org/package
set __TGZ_NAME=%__PACKAGE_NAME%.tar.gz
set __TGZ_URL=%__PACKAGE_URL%/%__PACKAGE_NAME%/!__TGZ_NAME!
set "__TGZ_FILE=%__TEMP_DIR%\!__TGZ_NAME!"
if %_DEBUG%==1 ( echo %_DEBUG_LABEL% powershell -c "Invoke-WebRequest -Uri !__TGZ_URL! -Outfile '!__TGZ_FILE!'" 1>&2
) else if %_VERBOSE%==1 ( echo Download archive file !__TGZ_NAME! to directory "%%TEMP%%\lib" 1>&2
)
powershell -c "$progressPreference='silentlyContinue';Invoke-WebRequest -Uri !__TGZ_URL! -Outfile '!__TGZ_FILE!'"
if not !ERRORLEVEL!==0 (
    echo %_ERROR_LABEL% Failed to download file "%__TGZ_NAME%" 1>&2
    set _EXITCODE=1
    goto :eof
)
pushd "%_LIB_DIR%"
if %_DEBUG%==1 ( echo %_DEBUG_LABEL% "%_TAR_CMD%" xf "%__TGZ_FILE%" 1>&2
) else if %_VERBOSE%==1 ( echo Extract archive file "%__TGZ_NAME%" into directory "!_LIB_DIR:%_ROOT_DIR%=!" 1>&2
)
call "%_TAR_CMD%" xf "%__TGZ_FILE%"
if not !ERRORLEVEL!==0 (
    popd
    echo %_ERROR_LABEL% Failed to extract file "%__TGZ_NAME%" into directory "!_LIB_DIR:%_ROOT_DIR%=!" 1>&2
    set _EXITCODE=1
    goto :eof
)
popd
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
    set __HADDOCK_OPTS=!__HADDOCK_OPTS! "--read-interface=!__PARENT_DIR!,%%f"
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
    echo %_ERROR_LABEL% Failed to generate HTML documentation into directory "!_TARGET_DOCS_DIR:%_ROOT_DIR%=!" 1>&2
    set _EXITCODE=1
    goto :eof
)
goto :eof

:run
set "__EXE_FILE=%_TARGET_DIR%\%_EXEC%.exe"
set "__EXE_ARGS=!_EXECUTABLE[%_EXECUTABLE_N%][args]!"
if not exist "%__EXE_FILE%" (
    echo %_ERROR_LABEL% Executable not found ^("!__EXE_FILE:%_ROOT_DIR%=!"^) 1>&2
    set _EXITCODE=1
    goto :eof
)
if %_DEBUG%==1 ( echo %_DEBUG_LABEL% "%__EXE_FILE%" %__EXE_ARGS% 1>&2
) else if %_VERBOSE%==1 ( echo Execute Haskell program "!__EXE_FILE:%_ROOT_DIR%=!" 1>&2
)
call "%__EXE_FILE%" %__EXE_ARGS%
if not %ERRORLEVEL%==0 (
    echo %_ERROR_LABEL% Failed to execute Haskell program "!__EXE_FILE:%_ROOT_DIR%=!" 1>&2
    set _EXITCODE=1
    goto :eof
)
goto :eof

@rem output parameter: _DURATION
:duration
set __START=%~1
set __END=%~2

for /f "delims=" %%i in ('call %"_PWSH_CMD%" -c "$interval = New-TimeSpan -Start '%__START%' -End '%__END%'; Write-Host $interval"') do set _DURATION=%%i
goto :eof

@rem #########################################################################
@rem ## Cleanups

:end
if %_TIMER%==1 (
    for /f "delims=" %%i in ('call %"_PWSH_CMD%" -c "(Get-Date)"') do set __TIMER_END=%%i
    call :duration "%_TIMER_START%" "!__TIMER_END!"
    echo Total execution time: !_DURATION! 1>&2
)
if %_DEBUG%==1 echo %_DEBUG_LABEL% _EXITCODE=%_EXITCODE% 1>&2
exit /b %_EXITCODE%
endlocal
