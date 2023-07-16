@echo off
if not defined IONHOME (
	set IONHOME=C:\Users\shmow\code\ion-compiler
)
if not exist build\ mkdir build
pushd build
cl /Zi /W2 /nologo /Feion "%~dp0main.c" /link /DEBUG:FULL
popd
