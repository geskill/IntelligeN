
set tool="C:\Program Files (x86)\Windows Kits\8.0\bin\x86\signtool.exe"

set integrated=%~dp0bin\
set portable=%~dp0bin_portable\
set sig=%~dp0sig\
set /p password=< "%sig%password.txt"

%tool% sign /fd SHA1 /f "%sig%IntelligeN.p12" /p %password% /tr http://timestamp.geotrust.com/tsa /td SHA1 "%portable%IntelligeN.exe"

%tool% sign /as /fd SHA256 /f "%sig%IntelligeN.p12" /p %password% /tr http://timestamp.geotrust.com/tsa /td SHA256 "%portable%IntelligeN.exe"

pause