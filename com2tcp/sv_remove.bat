@echo off
net session >NUL 2>nul
if %errorlevel% neq 0 (
 @powershell start-process %~0 -verb runas
 exit
)
cd %~dp0
nssm.exe stop "com2tcp"
nssm.exe remove "com2tcp" confirm
GOTO FIN
:ERR
ECHO FAIL!!
:FIN
timeout 2 > nul