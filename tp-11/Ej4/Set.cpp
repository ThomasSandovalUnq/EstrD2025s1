#include <string>
#include "Set.h"
using namespace std;

Set emptyS(){
    SetSt* setNuevo = new SetSt;
    setNuevo->cantidad = 0;
    setNuevo->primero = NULL;
    return setNuevo;
}

//Crea un conjunto vacío.
bool isEmptyS(Set s){
    return s->cantidad == 0;
}

//Indica si el conjunto está vacío.
bool belongsS(int x, Set s){
    NodoS* actual = s->primero;
    while (actual->elem != NULL)
    {
        if (actual->elem == x)
        {
            return true;
        }
        actual = actual->siguiente;
    }
    return false;
}

//Indica si el elemento pertenece al conjunto.
void AddS(int x, Set s){
    
    NodoS* actual = s->primero;
    while (actual->elem != NULL)
    {
        if (actual->elem == x)
        {
            return;
        }
        actual = actual->siguiente;
    }

    NodoS* nuevo = new NodoS;
    nuevo->elem = x;
    nuevo->siguiente = s->primero;
    s->primero = nuevo;
    s->cantidad++;    
}
//Agrega un elemento al conjunto.

void RemoveS(int x, Set s){
    NodoS* actual = s->primero;
    NodoS* anterior = nullptr;

    while (actual != nullptr) {
        if (actual->elem == x) {
            if (anterior == nullptr) {
                s->primero = actual->siguiente;
            } else {
                anterior->siguiente = actual->siguiente;
            }
            delete actual;
            s->cantidad--;
            return;
        }
        anterior = actual;
        actual = actual->siguiente;
    }
}
//Quita un elemento dado.

int sizeS(Set s){
    return s->cantidad;
}
//Devuelve la cantidad de elementos.

LinkedList setToList(Set s){
    
}
//Devuelve una lista con los ementos del conjunto.

void DestroyS(Set s){

}
//Libera la memoria ocupada por el conjunto.
