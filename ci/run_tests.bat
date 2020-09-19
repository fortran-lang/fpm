@echo on

cd fpm
if errorlevel 1 exit 1

fpm build
if errorlevel 1 exit 1

fpm run
if errorlevel 1 exit 1

fpm test
if errorlevel 1 exit 1

build\gfortran_debug\app\fpm
if errorlevel 1 exit 1

cd ..\test\example_packages\hello_world
if errorlevel 1 exit 1

..\..\..\fpm\build\gfortran_debug\app\fpm build
if errorlevel 1 exit 1

.\build\gfortran_debug\app\hello_world
if errorlevel 1 exit 1


cd ..\hello_complex
if errorlevel 1 exit 1

..\..\..\fpm\build\gfortran_debug\app\fpm build
if errorlevel 1 exit 1

.\build\gfortran_debug\app\say_Hello
if errorlevel 1 exit 1

.\build\gfortran_debug\app\say_goodbye
if errorlevel 1 exit 1

.\build\gfortran_debug\test\greet_test
if errorlevel 1 exit 1

.\build\gfortran_debug\test\farewell_test
if errorlevel 1 exit 1


cd ..\with_c
if errorlevel 1 exit 1

..\..\..\fpm\build\gfortran_debug\app\fpm build
if errorlevel 1 exit 1

.\build\gfortran_debug\app\with_c
if errorlevel 1 exit 1


cd ..\submodules
if errorlevel 1 exit 1

..\..\..\fpm\build\gfortran_debug\app\fpm build
if errorlevel 1 exit 1


cd ..\program_with_module
if errorlevel 1 exit 1

..\..\..\fpm\build\gfortran_debug\app\fpm build
if errorlevel 1 exit 1

.\build\gfortran_debug\app\Program_with_module
if errorlevel 1 exit 1