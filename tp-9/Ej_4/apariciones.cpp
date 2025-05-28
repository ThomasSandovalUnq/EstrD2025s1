#include <iostream>
using namespace std;

int apariciones(char c, string s){
    int contador = 0;
    for (char caracter : s){
        if (caracter == c) {
            contador++;
        }
    }
    return contador;
}

int aparicionesR(char c, string s){
    if (s.empty()){
        return 0;
    } else {
        int cuentaRestante = aparicionesR(c, s.substr(1));
        if (s[0] == c) {
            return 1 + cuentaRestante;
        } else {
            return cuentaRestante;
        }
    }
}

int main(){
    cout << apariciones('p', "pppppona") << endl;
    cout << aparicionesR('p', "ppppppona") << endl;
}