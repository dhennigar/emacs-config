del /q C:\Users\dhenn\.emacs.d\server\*
tasklist /fi "ImageName eq emacs.exe" /fo csv 2>NUL | find /I "emacs.exe">NUL
if NOT "%ERRORLEVEL%"=="0" "C:\Program Files\Emacs\emacs-29.1\bin\runemacs.exe" --daemon --chdir %HOMEPATH%
