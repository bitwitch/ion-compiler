@echo off
mkdir build
pushd build
cl -Zi -W2 -nologo ..\ion.c
popd
