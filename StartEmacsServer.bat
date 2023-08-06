REM Remove old server files
del /q C:\Users\dhenn\.emacs.d\server\*

REM Check if emacs is already running
tasklist /fi "ImageName eq emacs.exe" /fo csv 2>NUL | find /I "emacs.exe">NUL

REM If emacs is not running, start a new emacs server process
if NOT "%ERRORLEVEL%"=="0" "C:\Program Files\Emacs\emacs-29.1\bin\runemacs.exe" --daemon --chdir %HOMEPATH%
