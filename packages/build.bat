@echo off
setlocal
if not exist build\ mkdir build
pushd build

set IONPATH=%~dp0
if not exist SDL2.dll (
	xcopy "C:\Program Files\SDL2\lib\x64\SDL2.dll" .
)
set SDL_CFLAGS=/I"C:\Program Files\SDL2\include" /DSDL_MAIN_HANDLED
set SDL_LFLAGS=/LIBPATH:"C:\Program Files\SDL2\lib\x64"


if "%1" == "noir" (
	ion.exe noir
	cl /Zi /W3 /nologo %SDL_CFLAGS% noir.c SDL2.lib /link /DEBUG:FULL %SDL_LFLAGS%

) else if "%1" == "solitaire" (
	ion.exe solitaire
	cl /Zi /W3 /nologo %SDL_CFLAGS% solitaire.c SDL2.lib /link /DEBUG:FULL %SDL_LFLAGS%

) else if "%1" == "tictactoe" (
	ion.exe tictactoe
	cl /Zi /W3 /nologo tictactoe.c /link /DEBUG:FULL

) else if "%1" == "" (
	echo You must supply a package name to build
	popd
	exit /b 1

) else (
	echo Unknown package name: "%1"
	popd
	exit /b 1
)

popd
