rem Clean previous server file info first
del /q ""C:\\Users\\dhenn\\.emacs.d\\server\\*""
C:\Users\dhenn\scoop\apps\emacs\current\bin\runemacs.exe --daemon -c ""(setq default-directory "~/")""
rem start "" "C:\Users\&lt;user>\Desktop\emacsclientw.exe - Shortcut.lnk"
