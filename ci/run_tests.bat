@echo on

cd fpm
if errorlevel 1 exit 1

fpm build
if errorlevel 1 exit 1

fpm run
if errorlevel 1 exit 1

fpm run --args build
if errorlevel 1 exit 1

cd ..\test\example_packages\hello_world
if errorlevel 1 exit 1

..\..\..\fpm\fpm build
if errorlevel 1 exit 1

.\hello_world
if errorlevel 1 exit 1
