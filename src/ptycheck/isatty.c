#include <unistd.h>	//for isatty()
#include <stdio.h>	//for fileno()

#ifdef __MINGW64__
#include "iscygpty.h"
#endif

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