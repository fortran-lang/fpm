@echo on

cd fpm
if errorlevel 1 exit 1

fpm build
if errorlevel 1 exit 1

fpm run
if errorlevel 1 exit 1

build\gfortran_debug\app\fpm
if errorlevel 1 exit 1

cd ..\test\example_packages\hello_world
if errorlevel 1 exit 1

..\..\..\fpm\build\gfortran_debug\app\fpm build
if errorlevel 1 exit 1

.\build\gfortran_debug\hello_world
if errorlevel 1 exit 1
