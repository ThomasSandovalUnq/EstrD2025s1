#include "ArrayList.h"
using namespace std;

struct ArrayListSt {
int cantidad; // cantidad de elementos
int* elementos; // array de elementos
int capacidad; // tamaño del array
};

ArrayList newArrayList(){
    ArrayList al  = new ArrayListSt;
    al->cantidad  = 0;
    al->capacidad = 16;
    al->elementos = new int[al->capacidad];
    return al;
}

ArrayList newArrayListWith(int capacidad){
    //PRECOND: la capacidad dada, tiene que ser mayor o igual a 0.
    ArrayList al = new ArrayListSt;
    al->cantidad = 0;
    al->capacidad = capacidad;
    al->elementos = new int[capacidad];
    return al;
}

int lengthAL(ArrayList xs){
    return xs->cantidad;
}

int get(int i, ArrayList xs){
    return xs->elementos[i];
}

void set(int i, int x, ArrayList xs){
    xs->elementos[i] = x; 
}

