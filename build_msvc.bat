@echo off
setlocal enabledelayedexpansion

:: ============================================================================
:: BTL - MSVC Build Script (Windows)
::
:: Usage:
::   build_msvc.bat             Build btl.exe (interpreter)
::   build_msvc.bat transpiler  Build transpiler.exe
::   build_msvc.bat all         Build both
::   build_msvc.bat clean       Clean build artifacts
:: ============================================================================

set "BUILD_DIR=build"
set "BUILD_INTERP=0"
set "BUILD_TRANS=0"

:: Parse arguments
if "%~1"=="" set "BUILD_INTERP=1" & goto :args_done
if /i "%~1"=="transpiler" set "BUILD_TRANS=1" & goto :args_done
if /i "%~1"=="all" set "BUILD_INTERP=1" & set "BUILD_TRANS=1" & goto :args_done
if /i "%~1"=="clean" goto :clean
echo Unknown argument: %~1
exit /b 1
:args_done

:: --- Set up MSVC environment ---
set "VCVARS=C:\Program Files (x86)\Microsoft Visual Studio\2022\BuildTools\VC\Auxiliary\Build\vcvarsall.bat"
if not exist "%VCVARS%" (
    echo ERROR: vcvarsall.bat not found.
    exit /b 1
)
if "%VSCMD_VER%"=="" (
    echo Setting up MSVC environment...
    call "%VCVARS%" x64 >nul 2>&1
)
where cl.exe >nul 2>&1
if errorlevel 1 (
    echo ERROR: cl.exe not found.
    exit /b 1
)

if not exist "%BUILD_DIR%" mkdir "%BUILD_DIR%"

:: --- Common flags ---
set "CFLAGS=/nologo /O2 /W3 /utf-8 /MD /D_CRT_SECURE_NO_WARNINGS /Isrc"
set "LIBS="

:: ============================================================================
:: Build btl.exe (interpreter)
:: ============================================================================

if "%BUILD_INTERP%"=="1" (
    echo Building btl.exe...
    cl %CFLAGS% main.c src\*.c /Fe:btl.exe /link /nologo %LIBS%
    if errorlevel 1 (
        echo ERROR: btl.exe build failed.
        exit /b 1
    )
    :: Clean up obj files left in current directory
    del /q *.obj >nul 2>&1
    echo Built: btl.exe
)

:: ============================================================================
:: Build transpiler.exe
:: ============================================================================

if "%BUILD_TRANS%"=="1" (
    echo Building transpiler.exe...
    cl %CFLAGS% transpiler\transpiler.c transpiler\collect.c transpiler\transpiler_main.c src\*.c /Fe:"%BUILD_DIR%\transpiler.exe" /link /nologo %LIBS%
    if errorlevel 1 (
        echo ERROR: transpiler.exe build failed.
        exit /b 1
    )
    :: Clean up obj files left in current directory
    del /q *.obj >nul 2>&1
    echo Built: %BUILD_DIR%\transpiler.exe
)

echo Done.
exit /b 0

:clean
echo Cleaning...
if exist btl.exe del /q btl.exe
if exist "%BUILD_DIR%\transpiler.exe" del /q "%BUILD_DIR%\transpiler.exe"
if exist "%BUILD_DIR%\generated.c" del /q "%BUILD_DIR%\generated.c"
del /q *.obj >nul 2>&1
echo Clean complete.
exit /b 0
