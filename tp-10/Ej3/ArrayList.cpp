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

void resize(int capacidad, ArrayList xs){
    if (xs->capacidad == capacidad){
    }
    else {
        ArrayList al = new ArrayListSt;
        al->cantidad = xs->cantidad;
        al->capacidad = capacidad;
        al->elementos = xs->elementos;
        delete xs;}
}

void DuplicarTamanioDeArray(ArrayList xs){
    int* temp = new int[xs->capacidad*2];
    for(int i=0; i<xs->capacidad; i++){
        temp[i] = xs->elementos[i];
    }
    delete xs->elementos;
    xs->capacidad = xs->capacidad*2;
    xs->elementos = temp;
    delete temp;
}

void add(int x, ArrayList xs){
    if (xs->cantidad == xs->capacidad){
        DuplicarTamanioDeArray(xs);
    }
    xs->elementos[xs->cantidad++] = x;
}

void remove(ArrayList xs){
    if (xs->cantidad > 0){
        xs->cantidad--;
    }
}