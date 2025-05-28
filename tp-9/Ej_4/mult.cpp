#include <iostream>
using namespace std;

int mult(int n, int m){
    if (m == 0) return 0;
    int resultado = n;
    while (m > 1) {
        resultado = resultado + n;
        m = m - 1;
    };
    return resultado;
}

int multR(int n, int m){
    if (m == 0) return 0;
    if (m > 1) {
        return n + multR(n, m - 1);
    }
}

int main(){
    cout << "Iterativa: 3 * 0 = " << mult(3, 0) << endl;
    cout << "Recursiva: 3 * 0 = " << multR(3, 0) << endl;
}