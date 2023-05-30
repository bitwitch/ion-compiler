@echo off
..\build\ion.exe tictactoe.ion > ttt.c
cl /nologo /Zi ttt.c

