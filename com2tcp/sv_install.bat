@echo off
net session >NUL 2>nul
if %errorlevel% neq 0 (
 @powershell start-process %~0 -verb runas
 exit
)
cd /d "%~dp0"
nssm.exe stop "com2tcp"
nssm.exe remove "com2tcp" confirm
echo -------------
:IP
SET TARGETIP=
SET /P TARGETIP="Destination IP or hostname=
IF "%TARGETIP%"=="" GOTO :ERR
:PORT
SET TARGETPORT=
SET /P TARGETPORT="Destination port=
IF "%TARGETPORT%"=="" GOTO :ERR

:: ループ動作用のラッパーバッチを同階層に生成
(
echo @echo off
echo :LOOP
echo "%~dp0com2tcp.exe" --terminal lsrmst --ignore-dsr \\.\CNCB0 %TARGETIP% %TARGETPORT%
echo ping 127.0.0.1 -n 2 ^>nul
echo goto :LOOP
) > "%~dp0com2tcp_run.bat"

:: NSSMにラッパーバッチを直接登録
:: ※NSSMが自動的に cmd.exe 経由で実行し、作業ディレクトリもバッチの場所に合わせます
nssm.exe install "com2tcp" "%~dp0com2tcp_run.bat"
nssm.exe set "com2tcp" AppPriority ABOVE_NORMAL_PRIORITY_CLASS
nssm.exe set "com2tcp" AppStdout NUL
nssm.exe set "com2tcp" AppStderr NUL
nssm.exe start "com2tcp"
ECHO SUCCESS!!
GOTO FIN
:ERR
ECHO FAIL!!
:FIN
timeout 2 > nul