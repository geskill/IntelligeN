
set integrated="%~dp0bin\"
set portable="%~dp0bin_portable\"

cd /d %portable%

if errorlevel 1 exit /b

for /F "delims=" %%i in ('dir /b') do if not "%%i"=="IntelligeN.dll" (rmdir "%%i" /s/q || del "%%i" /s/q)

