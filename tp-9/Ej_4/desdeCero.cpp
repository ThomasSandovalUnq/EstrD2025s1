#include <iostream>
using namespace std;

void desdeCeroHastaN(int n){
    int x = 0;
    while (x <= n){
        cout << x << endl;
        x++;
    }
}

void desdeCeroHastaNR(int n){
    if (n < 0) return ;
        desdeCeroHastaNR(n-1);
        cout << n << endl;
}

int main(){
    cout << "Iterativa:" << endl;
    desdeCeroHastaN(3);
    cout << "Recursiva:" << endl;
    desdeCeroHastaNR(3);
}