@echo off
mkdir build
pushd build
cl -Zi -W2 -nologo ..\simple_parser.c
popd
