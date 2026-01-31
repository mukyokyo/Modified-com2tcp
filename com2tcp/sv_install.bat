@echo off
net session >NUL 2>nul
if %errorlevel% neq 0 (
 @powershell start-process %~0 -verb runas
 exit
)
:IP
SET TARGETIP=
SET /P TARGETIP="Destination IP or hostname=
IF "%TARGETIP%"=="" GOTO :ERR
:PORT
SET TARGETPORT=
SET /P TARGETPORT="Destination port=
IF "%TARGETPORT%"=="" GOTO :ERR
nssm.exe install "com2tcp" "C:\src\EMB\W32\com2tcp\com2tcp.exe" "--terminal lsrmst --ignore-dsr \\.\CNCB0 %TARGETIP% %TARGETPORT%"
nssm.exe set "com2tcp" AppPriority ABOVE_NORMAL_PRIORITY_CLASS
nssm.exe start "com2tcp"
GOTO FIN
:ERR
ECHO FAIL
:FIN
timeout 2 > nul