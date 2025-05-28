#include <iostream>
using namespace std;
#include "Par.h"


int main(){
    Par p1 = consPar(10, 3);

    cout << "fst(p1): " << fst(p1) << endl;
    cout << "snd(p1): " << snd(p1) << endl;
    cout << "maxDelPar(p1): " << maxDelPar(p1) << endl;

    Par p2 = swap(p1);
    cout << "swap(p1): (" << fst(p2) << ", " << snd(p2) << ")" << endl;

    Par p3 = divisionYResto(10, 3);
    cout << "divisionYResto(10, 3): (" << fst(p3) << ", " << snd(p3) << ")" << endl;

    return 0;
}