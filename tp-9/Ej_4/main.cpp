#include <iostream>
using namespace std;

void printN(int n, string s){
    while (n > 0){
        cout << s << endl;
        n --;
    }
}

void printNR(int n, string s){
    if (n > 0){
        cout << s << endl;
        printNR(n-1, s);
    }
}

int main(){
    cout << "Iterativa:"<< endl;
    printN(2, "PERSONA");
    cout << "Recursiva:" << endl;
    printNR(2, "PERSONA");
}