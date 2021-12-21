#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main(int argc, char **argv)
{
    clock_t start = clock();
    srand(time(NULL));
    int randomList[500000];

    for (int i = 0; i < 500000; i++)
    {
        randomList[i] = (rand() % 9) + 1;
    }

    for (int i = 0; i < 500000; i++)
    {
        randomList[i] = randomList[i] + 1; 
    }

    int sum = 0;
    for (int i = 0; i < 500000; i++)
    {
        sum += randomList[i];
    }

    printf("Sum is: %d\n", sum);
    clock_t stop = clock();
    double totalTime = (double)(stop - start) * 1000/CLOCKS_PER_SEC;
    printf("Total time is: %f ms \n", totalTime);
}
