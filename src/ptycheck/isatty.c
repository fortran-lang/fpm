// This file provides a `c_isatty` wrapper function to check if `stdout` is connected
// to a terminal or not. This wrapper is required for better portability, specifically
// for supporting the MS Windows command prompt and the MinTTY terminal used by MSYS2.

#include <unistd.h>	//for isatty()
#include <stdio.h>	//for fileno()

#ifdef __MINGW64__
// ptycheck/iscygpty allows us to check if connected to MinTTY in MSYS2 on Windows
#include "iscygpty.h"
#endif

// Check if `stdout` is connected to a terminal
// Returns 1 if is a terminal, and 0 otherwise
int c_isatty(void)
{

    if (isatty(fileno(stdout))){
        return 1;
    } else {

        #ifdef __MINGW64__
        if (is_cygpty(fileno(stdout))){
            return 1;
        } else {
            return 0;
        }
        #endif

        return 0;
    }
    
}