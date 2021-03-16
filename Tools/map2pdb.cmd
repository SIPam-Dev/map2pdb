@echo off

if [%1]==[] goto usage
if not exist "%~dp0\llvm-pdbutil.exe" goto llvm-pdbutil_missing

echo:
echo Converting MAP to YAML...

"%~dp0\..\Bin\Win32\Debug\map2yaml" %1.map

if errorlevel 1 goto exit

echo:
echo Converting YAML to PDB...

"%~dp0\llvm-pdbutil" yaml2pdb %1.yaml

if errorlevel 1 goto exit

echo:
echo Binding PDB to EXE...

"%~dp0\..\Bin\Win32\Debug\bindpdb" %1.exe

goto exit

:usage
echo:
echo Usage: map2pdb ^<name^>
echo:
echo Produces ^<name^>.pdb from ^<name^>.map and patches ^<name^>.exe to reference the pdb file.
echo:
echo Note: Do not specify the .exe file type in the name.
goto exit

:llvm-pdbutil_missing
echo:
rem echo llvm-pdbutil.exe not found in %~dp0
echo:
rem echo %~dp0\readme.txt contains the download location.
echo
goto exit

:exit

