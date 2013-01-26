@echo off
SET THEFILE=SimThyr.exe
echo Linking %THEFILE%
c:\lazarus\fpc\2.6.0\bin\i386-win32\ld.exe -b pei-i386 -m i386pe  -macosx_version_min 10.4 --gc-sections   --subsystem windows --entry=_WinMainCRTStartup    -o SimThyr.exe link.res
if errorlevel 1 goto linkend
c:\lazarus\fpc\2.6.0\bin\i386-win32\postw32.exe --subsystem gui --input SimThyr.exe --stack 16777216
if errorlevel 1 goto linkend
goto end
:asmend
echo An error occured while assembling %THEFILE%
goto end
:linkend
echo An error occured while linking %THEFILE%
:end
