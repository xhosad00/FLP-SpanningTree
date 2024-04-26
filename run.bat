@echo off
@REM swipl -q -g start -o flp23-log -c spanning-Tree.pl 
swipl -q -g start -o flp23-log -c spanning-Tree.pl 
@REM if errorlevel 1 (
@REM     echo Compilation failed.
@REM     exit /b 1
@REM )

@REM if not exist flp23-log.exe (
@REM     echo Compilation failed: flp23-log.exe not found.
@REM     exit /b 1
@REM )

@REM if not exist in.txt (
@REM     echo Input file 'in.txt' not found.
@REM     exit /b 1
@REM )

flp23-log.exe < in.txt
