#include <iostream>
using namespace std;

//PRECOND: EL STRING TIENE AL MENOS n CHAR.
void primerosN(int n, string s){
    for (int i = 0; i < n; ++i) {
        cout << s[i] << endl;
    }
}

void primerosNR(int n, string s){
    if (n > 0){
        cout << s[0] << endl;
        primerosNR(n-1, s.substr(1));
    }
}
//esta mal, doble recursion!

int main() {
    cout << "Iterativo:" << endl;
    primerosN(5, "Homero Chino");
    cout << "Recursivo:" << endl;
    primerosNR(5, "Homero Chino");
}