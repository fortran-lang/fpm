@echo on

cd fpm
if errorlevel 1 exit 1

fpm build %*
if errorlevel 1 exit 1

fpm run %*
if errorlevel 1 exit 1

fpm run %* -- --help
if errorlevel 1 exit 1

fpm run %* -- --version
if errorlevel 1 exit 1

rmdir fpm_scratch_* /s /q
fpm test %*
if errorlevel 1 exit 1
rmdir fpm_scratch_* /s /q

for /f %%i in ('fpm run %* --runner echo') do set fpm_path=%%i
echo %fpm_path%

%fpm_path%
if errorlevel 1 exit 1

%fpm_path% build
if errorlevel 1 exit 1

%fpm_path% install --prefix "%CD%\_dist" --no-rebuild
if errorlevel 1 exit 1

cd ..\example_packages\hello_world
if errorlevel 1 exit 1

del /q /f build
%fpm_path% build
if errorlevel 1 exit 1

%fpm_path% run --target hello_world
if errorlevel 1 exit 1

%fpm_path% run
if errorlevel 1 exit 1


cd ..\hello_fpm
if errorlevel 1 exit 1

del /q /f build
%fpm_path% build
if errorlevel 1 exit 1

%fpm_path% run --target hello_fpm
if errorlevel 1 exit 1


cd ..\circular_test
if errorlevel 1 exit 1

del /q /f build
%fpm_path% build
if errorlevel 1 exit 1


cd ..\circular_example
if errorlevel 1 exit 1

del /q /f build
%fpm_path% build
if errorlevel 1 exit 1


cd ..\hello_complex
if errorlevel 1 exit 1

del /q /f build
%fpm_path% build
if errorlevel 1 exit 1

%fpm_path% test
if errorlevel 1 exit 1

%fpm_path% run --target say_Hello
if errorlevel 1 exit 1

%fpm_path% run --target say_goodbye
if errorlevel 1 exit 1

%fpm_path% test --target greet_test
if errorlevel 1 exit 1

%fpm_path% test --target farewell_test
if errorlevel 1 exit 1


cd ..\hello_complex_2
if errorlevel 1 exit 1

del /q /f build
%fpm_path% build
if errorlevel 1 exit 1

%fpm_path% run --target say_hello_world
if errorlevel 1 exit 1

%fpm_path% run --target say_goodbye
if errorlevel 1 exit 1

%fpm_path% test --target greet_test
if errorlevel 1 exit 1

%fpm_path% test --target farewell_test


cd ..\with_examples
if errorlevel 1 exit 1

del /q /f build
%fpm_path% build
if errorlevel 1 exit 1

%fpm_path% run --example --target demo-prog
if errorlevel 1 exit 1

%fpm_path% run --target demo-prog
if errorlevel 1 exit 1


cd ..\auto_discovery_off
if errorlevel 1 exit 1

del /q /f build
%fpm_path% build
if errorlevel 1 exit 1

%fpm_path% run --target auto_discovery_off
if errorlevel 1 exit 1

%fpm_path% test --target my_test
if errorlevel 1 exit 1

if exist .\build\gfortran_*\app\unused exit /B 1

if exist .\build\gfortran_*\test\unused_test exit /B 1


cd ..\with_c
if errorlevel 1 exit 1

del /q /f build
%fpm_path% build
if errorlevel 1 exit 1

%fpm_path% run --target with_c
if errorlevel 1 exit 1


cd ..\submodules
if errorlevel 1 exit 1

del /q /f build
%fpm_path% build
if errorlevel 1 exit 1


cd ..\program_with_module
if errorlevel 1 exit 1

del /q /f build
%fpm_path% build
if errorlevel 1 exit 1

%fpm_path% run --target Program_with_module
if errorlevel 1 exit 1


cd ..\link_executable
if errorlevel 1 exit 1

del /q /f build
%fpm_path% build
if errorlevel 1 exit 1

%fpm_path% run --target gomp_test
if errorlevel 1 exit 1


cd ..\fortran_includes
if errorlevel 1 exit 1

del /q /f build
%fpm_path% build
if errorlevel 1 exit 1


cd ..\c_includes
if errorlevel 1 exit 1

del /q /f build
%fpm_path% build
if errorlevel 1 exit 1


cd ..\c_header_only
if errorlevel 1 exit 1

del /q /f build
%fpm_path% build
if errorlevel 1 exit 1

cd ..\..
