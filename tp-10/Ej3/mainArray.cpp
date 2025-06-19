#include "ArrayList.h"
using namespace std;
#include <iostream>

int sumatoria(ArrayList xs){
    int contador = 0;
    for (int i=0; i<lengthAL(xs); i++){
        contador = contador + get(i, xs);
    }
    return contador;
}

//g++ -o app cpps 

/*int main(){
    ArrayList lNum = newArrayList();
    add(2,lNum);
    add(3,lNum);
    add(4,lNum);
    cout << sumatoria(lNum) << endl;
}*/

ArrayList sucesores(ArrayList xs){
    for (int i=0; i<lengthAL(xs); i++){
        set(i, get(i, xs), xs);
    }
    return xs;
}

int main(){
    ArrayList lNum = newArrayList();
    add(2,lNum);
    add(3,lNum);
    add(4,lNum);
    sucesores(lNum);
    cout << sucesores(lNum) << endl;
}