@echo off
mkdir build
pushd build
cl -Zi -W2 -nologo ..\main.c
popd
