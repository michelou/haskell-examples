@echo off
setlocal enabledelayedexpansion

set _DEBUG=0

rem ##########################################################################
rem ## Environment setup

set _BASENAME=%~n0

set _EXITCODE=0

for %%f in ("%~dp0") do set "_ROOT_DIR=%%~f"

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
if %_CLEAN%==1 (
    call :clean
    if not !_EXITCODE!==0 goto end
)
if %_COMPILE%==1 (
    call :compile_%_TARGET%
    if not !_EXITCODE!==0 goto end
)
if %_DOC%==1 (
    call :doc
    if not !_EXITCODE!==0 goto end
)
if %_RUN%==1 (
    call :run_%_TARGET%
    if not !_EXITCODE!==0 goto end
)
goto end

rem ##########################################################################
rem ## Subroutine

rem output parameters: _DEBUG_LABEL, _ERROR_LABEL, _WARNING_LABEL
rem                    _SOURCE_FILES, MAIN_CLASS, _EXE_FILE
:env
rem ANSI colors in standard Windows 10 shell
rem see https://gist.github.com/mlocati/#file-win10colors-cmd
set _DEBUG_LABEL=[46m[%_BASENAME%][0m
set _ERROR_LABEL=[91mError[0m:
set _WARNING_LABEL=[93mWarning[0m:

set "_APP_DIR=%_ROOT_DIR%app"
set "_TARGET_DIR=%_ROOT_DIR%target"
set "_TARGET_GEN_DIR=%_TARGET_DIR%\gen"
set "_DOCS_DIR=%_TARGET_DIR%\docs"

set _MAIN_NAME=Main
set "_EXE_FILE=%_TARGET_DIR%\%_MAIN_NAME%.exe"

set _GHC_CMD=ghc.exe
rem option "-hidir <dir>" redirects all generated interface files into <dir>
set _GHC_OPTS=-Wall -Werror -o "%_EXE_FILE%" -hidir "%_TARGET_GEN_DIR%" -odir "%_TARGET_GEN_DIR%"

set _HADDOCK_CMD=haddock.exe
set _HADDOCK_OPTS=--odir="%_DOCS_DIR%" --html --title=%_MAIN_NAME% --package-name=Main
goto :eof

rem input parameter: %*
rem output parameter(s): _CLEAN, _COMPILE, _DEBUG, _RUN, _TIMER, _VERBOSE
:args
set _CLEAN=0
set _COMPILE=0
set _DOC=0
set _HELP=0
set _RUN=0
set _TARGET=native
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
    rem option
    if /i "%__ARG%"=="-debug" ( set _DEBUG=1
    ) else if /i "%__ARG%"=="-help" ( set _HELP=1
    ) else if /i "%__ARG%"=="-timer" ( set _TIMER=1
    ) else if /i "%__ARG%"=="-verbose" ( set _VERBOSE=1
    ) else (
        echo %_ERROR_LABEL% Unknown option %__ARG% 1>&2
        set _EXITCODE=1
        goto args_done
   )
) else (
    rem subcommand
    set /a __N+=1
    if /i "%__ARG%"=="clean" ( set _CLEAN=1
    ) else if /i "%__ARG%"=="compile" ( set _COMPILE=1
    ) else if /i "%__ARG%"=="doc" ( set _DOC=1
    ) else if /i "%__ARG%"=="help" ( set _HELP=1
    ) else if /i "%__ARG%"=="run" ( set _COMPILE=1& set _RUN=1
    ) else (
        echo %_ERROR_LABEL% Unknown subcommand %__ARG% 1>&2
        set _EXITCODE=1
        goto args_done
    )
)
shift
goto :args_loop
:args_done
if %_DEBUG%==1 ( set _REDIRECT_STDOUT=1^>CON
) else ( set _REDIRECT_STDOUT=1^>NUL
)
if %_DEBUG%==1 echo %_DEBUG_LABEL% _CLEAN=%_CLEAN% _COMPILE=%_COMPILE% _DOC=%_DOC% _RUN=%_RUN% _VERBOSE=%_VERBOSE%
if %_TIMER%==1 for /f "delims=" %%i in ('powershell -c "(Get-Date)"') do set _TIMER_START=%%i
goto :eof

:help
echo Usage: %_BASENAME% { ^<option^> ^| ^<subcommand^> }
echo.
echo   Options:
echo     -debug      show commands executed by this script
echo     -timer      display total elapsed time
echo     -verbose    display progress messages
echo.
echo   Subcommands:
echo     clean       delete generated files
echo     compile     generate program executable
echo     doc         generate HTML documentation
echo     help        display this help message
echo     run         execute the generated program
goto :eof

:clean
if not exist "%_TARGET_DIR%\" goto :eof
if %_DEBUG%==1 ( echo %_DEBUG_LABEL% rmdir /s /q "%_TARGET_DIR%" 1>&2
) else if %_VERBOSE%==1 ( echo Remove directory %_TARGET_DIR% 1>&2
)
rmdir /s /q "%_TARGET_DIR%"
if not %ERRORLEVEL%==0 (
    set _EXITCODE=1
    goto :eof
)
goto :eof

:compile_native
if not exist "%_TARGET_DIR%" mkdir "%_TARGET_DIR%"

set __SOURCE_FILES=
for /f "usebackq delims=" %%f in (`where /r "%_APP_DIR%" *.hs`) do (
    set __SOURCE_FILES=!__SOURCE_FILES! "%%f"
)
if %_DEBUG%==1 ( echo %_DEBUG_LABEL% %_GHC_CMD% %_GHC_OPTS% %__SOURCE_FILES% 1>&2
) else if %_VERBOSE%==1 ( echo Compile Haskell source files 1>&2
)
call %_GHC_CMD% %_GHC_OPTS% %__SOURCE_FILES% %_REDIRECT_STDOUT%
if not %ERRORLEVEL%==0 (
   set _EXITCODE=1
   goto :eof
)
goto :eof

:doc
if not exist "%_DOCS_DIR%" mkdir "%_DOCS_DIR%"

set __SOURCE_FILES=
for /f "usebackq delims=" %%f in (`where /r "%_APP_DIR%" *.hs`) do (
    set __SOURCE_FILES=!__SOURCE_FILES! "%%f"
)
if %_DEBUG%==1 ( echo %_DEBUG_LABEL% %_HADDOCK_CMD% %_HADDOCK_OPTS% %__SOURCE_FILES% 1>&2
) else if %_VERBOSE%==1 ( echo Generate Haskell documentation into directory !_DOCS_DIR:%_ROOT_DIR%=! 1>&2
)
call %_HADDOCK_CMD% %_HADDOCK_OPTS% %__SOURCE_FILES%
if not %ERRORLEVEL%==0 (
   set _EXITCODE=1
   goto :eof
)
goto :eof

:run_native
if not exist "%_EXE_FILE%" (
    set _EXITCODE=1
	goto :eof
)
if %_DEBUG%==1 ( echo %_DEBUG_LABEL% %_EXE_FILE% 1>&2
) else if %_VERBOSE%==1 ( echo Execute Haskell program !_EXE_FILE:%_ROOT_DIR%=! 1>&2
)
call "%_EXE_FILE%"
if not %ERRORLEVEL%==0 (
   set _EXITCODE=1
   goto :eof
)
goto :eof

rem output parameter: _DURATION
:duration
set __START=%~1
set __END=%~2

for /f "delims=" %%i in ('powershell -c "$interval = New-TimeSpan -Start '%__START%' -End '%__END%'; Write-Host $interval"') do set _DURATION=%%i
goto :eof

rem ##########################################################################
rem ## Cleanups

:end
if %_TIMER%==1 (
    for /f "delims=" %%i in ('powershell -c "(Get-Date)"') do set __TIMER_END=%%i
    call :duration "%_TIMER_START%" "!__TIMER_END!"
    echo Elapsed time: !_DURATION! 1>&2
)
if %_DEBUG%==1 echo %_DEBUG_LABEL% _EXITCODE=%_EXITCODE% 1>&2
exit /b %_EXITCODE%
endlocal
