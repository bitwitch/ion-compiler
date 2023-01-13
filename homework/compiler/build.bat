@echo off
mkdir build
pushd build
cl -Zi -W2 -nologo -Fequickmafs ..\main.c
popd
