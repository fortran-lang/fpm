#include <unistd.h>	//for isatty()
#include <stdio.h>	//for fileno()
 
int c_isatty(void)
{

	if (isatty(fileno(stdin))){
        return 1;
    } else {
        return 0;
    }
    
}