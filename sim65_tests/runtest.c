
#include <stdio.h>
#include <stdlib.h>

#include "test.h"

int main(void)
{
    for (;;)
    {
        printf("test result: %u\n", test());
    }
    return EXIT_SUCCESS;
}
