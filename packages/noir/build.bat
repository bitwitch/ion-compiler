@echo off
if not exist build\ mkdir build
pushd build

setlocal EnableExtensions EnableDelayedExpansion
set "_path=%~dp0"
set SRC_DIR=%_path:~0,-1%
set ION_DIR=%SRC_DIR%\..
set URL=https://github.com/libsdl-org/SDL/releases/download/release-2.26.5/SDL2-devel-2.26.5-VC.zip
set ZIPFILE="%CD%\SDL2-devel-2.26.5-VC.zip"

REM copy SDL2 libraries to build directory if they haven't already been
if not exist SDL2\ (
	if exist %SRC_DIR%\SDL2\ (
		xcopy /D /E /I %SRC_DIR%\SDL2\include SDL2\include\SDL2
		echo F | xcopy /D %SRC_DIR%\SDL2\lib\x64\SDL2.lib SDL2\lib\SDL2.lib
		echo F | xcopy /D %SRC_DIR%\SDL2\lib\x64\SDL2.dll .

REM download SDL2 development libraries
	) else (
		bitsadmin /transfer downloadSDL2 /download %URL% %ZIPFILE%
		if not exist %ZIPFILE% (
			echo Build Error: Failed to download SDL2. 
			echo You can manually download SDL2 development libraries for VC, extract the contents, and move them to a directory called SDL2 in the project root.
			echo https://github.com/libsdl-org/SDL/releases/
			endlocal
			popd
			exit /b 1
		)

REM unzip downloaded SDL2 zip file
		powershell -nologo -noprofile -command "Expand-Archive -Path '%ZIPFILE%' -DestinationPath '%SRC_DIR%'"
		if not exist %SRC_DIR%\SDL2-2.26.5 (	
			echo "Failed to unzip %ZIPFILE%"
			echo "You can manually extract the contents, and move them to a directory called SDL2 in the project root."
			endlocal
			popd
			exit /b 1
		)
		move %SRC_DIR%\SDL2-2.26.5 %SRC_DIR%\SDL2
		del %ZIPFILE%

REM copy SDL2 libraries to build directory
		xcopy /D /E /I %SRC_DIR%\SDL2\include SDL2\include\SDL2
		echo F | xcopy /D %SRC_DIR%\SDL2\lib\x64\SDL2.lib SDL2\lib\SDL2.lib
		echo F | xcopy /D %SRC_DIR%\SDL2\lib\x64\SDL2.dll .
	)
)

REM compile noir.ion
"%ION_DIR%\build\ion.exe" "%SRC_DIR%\noir.ion"

REM compile main program
cl /Zi /nologo /W3 /DSDL_MAIN_HANDLED /D_CRT_SECURE_NO_WARNINGS /ISDL2\include /Fenoir "%SRC_DIR%\noir_impl.c" /link /LIBPATH:SDL2\lib SDL2.lib

endlocal
popd
