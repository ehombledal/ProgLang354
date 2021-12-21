#include <iostream>
#include <vector>
#include <cstdlib>
#include <ctime>
#include <numeric>

using namespace std;

int main()
{
    clock_t start = clock();
    vector<int> randomList(500000);
    srand(time(NULL));

    for (int i = 0; i < 500000; i++)
    {
        randomList[i] = (rand() % 9) + 1;
    }

    for (int i = 0; i < 500000; i++)
    {
        randomList[i] += 1; 
    }

    cout << "sum of elements is: " << accumulate(randomList.begin(), randomList.end(), 0) << endl;
    clock_t stop = clock();
    double totalTime = (double)(stop - start) * 1000/CLOCKS_PER_SEC;
    cout << "Total time is: " << totalTime << " ms" << endl; 
}