rem Sets HOME for current shell
set HOME=%APPDATA%

rem Clean previous server file info first
del /q ""%HOME%\\.emacs.d\\server\\*""
C:\emacs-27.1-x86_64\bin\runemacs.exe --daemon -c ""(setq default-directory "~/")""
rem start "" "C:\Users\&lt;user>\Desktop\emacsclientw.exe - Shortcut.lnk"
