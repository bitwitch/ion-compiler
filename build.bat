@echo off
if not exist build\ mkdir build
pushd build
cl /Zi /W2 /nologo /Feion "%~dp0main.c"
popd
