#include <iostream>
using namespace std;

void cuentaRegresiva(int n){
    while (n >= 0)
    {
        cout << n << endl;
        n--;
    }
}

void cuentaRegresivaR(int n){
    if (n >= 0) {
        cout << n << endl;
        cuentaRegresivaR(n-1);
    }
}

int main(){
    cout<< "Iterativa:" << endl;
    cuentaRegresiva(3);
    cout<< "Recursiva:" << endl;
    cuentaRegresivaR(3);
}