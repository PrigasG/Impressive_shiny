@echo off
setlocal enabledelayedexpansion

REM ==== Base directory of this launcher (LTC file checker folder) ====
set "RootDir=%~dp0"

REM ==== Bundled Pandoc (optional) ====
set "PANDOC_DIR=%RootDir%pandoc"
if exist "%PANDOC_DIR%\pandoc.exe" (
  set "PATH=%PANDOC_DIR%;%PATH%"
  set "RSTUDIO_PANDOC=%PANDOC_DIR%"
  echo Using bundled Pandoc: "%PANDOC_DIR%"
) else (
  echo NOTE: Bundled Pandoc not found at "%PANDOC_DIR%". (OK unless you render PDFs via rmarkdown.)
)

REM ==== App folder ====
set "AppDir=%RootDir%app"
if not exist "%AppDir%" (
  echo ERROR: App folder not found at "%AppDir%".
  pause
  exit /b 1
)

REM ==== Prepare logging (write to TEMP so it always works) ====
set "LogDir=%TEMP%\OLTCR_FileChecker_logs"
if not exist "%LogDir%" mkdir "%LogDir%"

REM Safer timestamp (no locale issues)
for /f %%I in ('powershell -NoProfile -Command "Get-Date -Format yyyyMMdd_HHmmss"') do set "stamp=%%I"

set "LOG=%LogDir%\app_%stamp%.log"

echo [debug] Writing log to: "%LOG%"
echo [debug] RootDir="%RootDir%"
echo [debug] AppDir="%AppDir%"
echo [debug] RS="%RS%"
echo [debug] R="%R%"

(
  echo [START] %date% %time%
  echo RootDir="%RootDir%"
  echo AppDir="%AppDir%"
  echo HUB_TEAM_ROOT="%HUB_TEAM_ROOT%"
  echo HUB_PROJECT_FILES="%HUB_PROJECT_FILES%"
  echo HUB_DATA_SRC_SP="%HUB_DATA_SRC_SP%"
  echo HUB_DATA_PATH="%HUB_DATA_PATH%"
  if defined RS (echo RS="%RS%") else (echo R="%R%")
) > "%LOG%" 2>&1

if not exist "%LOG%" (
  echo [FATAL] Could not create log file. Check permissions or antivirus.
  pause
  exit /b 1
)


REM ==== R detection ====
set "RPathFound=false"
set "RS="
set "R="

REM Prefer portable Rscript.exe (bundled)
for %%P in (
  "%RootDir%R\bin\Rscript.exe"
  "%RootDir%R\bin\x64\Rscript.exe"
  "%RootDir%R-Portable\bin\Rscript.exe"
  "%RootDir%R-Portable\bin\x64\Rscript.exe"
  "%RootDir%R-Portable\App\R-Portable\bin\Rscript.exe"
  "%RootDir%R-Portable\App\R-Portable\bin\x64\Rscript.exe"
) do (
  if exist "%%~fP" (
    set "RS=%%~fP"
    set "RPathFound=true"
    goto :prep
  )
)

REM Portable R.exe fallback
if exist "%RootDir%R\bin\x64\R.exe" (
  set "R=%RootDir%R\bin\x64\R.exe"
  set "RPathFound=true"
  goto :prep
)

REM System R under Program Files
for %%d in ("C:\Program Files\R" "C:\Program Files (x86)\R") do (
  for /d %%i in (%%d\R-*) do (
    if exist "%%i\bin\Rscript.exe" (
      set "RS=%%i\bin\Rscript.exe"
      set "RPathFound=true"
      goto :prep
    ) else (
      if exist "%%i\bin\x64\R.exe" (
        set "R=%%i\bin\x64\R.exe"
        set "RPathFound=true"
        goto :prep
      )
    )
  )
)

REM Prompt if still not found
if not "%RPathFound%"=="true" (
  set /p "R=Cannot detect R installation. Enter full path to R.exe: "
  if not exist "%R%" (
    echo The specified path does not exist. Exiting.
    pause
    exit /b 1
  )
)

:prep
REM ==== Run app ====
if defined RS (
  echo Launching with Rscript... >> "%LOG%"
  "%RS%" --vanilla "%RootDir%run.R" "%AppDir%" >> "%LOG%" 2>&1
) else (
  echo Launching with R.exe... >> "%LOG%"
  "%R%" --no-save --slave -f "%RootDir%run.R" --args "%AppDir%" >> "%LOG%" 2>&1
)

set "RC=%errorlevel%"
echo [END] %date% %time% (exitcode=%RC%) >> "%LOG%"

echo.
echo ===== Finished. Exit code: %RC% =====
echo Log location: "%LOG%"
echo Open it if nothing launched.
pause
exit /b %RC%

