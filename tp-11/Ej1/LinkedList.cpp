#include <string>
#include "LinkedList.h"
using namespace std;

LinkedList nil(){
    LinkedListSt* l = new LinkedListSt;
    l->cantidad = 0;
    l->primero = NULL;
    return l;
}

bool isEmpty(LinkedList xs){
    return xs->cantidad == 0;
}

int head(LinkedList xs){
//PRECOND: LA LISTA NO PUEDE SER VACIA
    return (xs->primero->elem);
}

void Cons(int x, LinkedList xs){
    NodoL* nodo = new NodoL;
    nodo->elem = x;
    nodo->siguiente = xs->primero;
    xs->primero = nodo;
    xs->cantidad++;
}

void Tail(LinkedList xs){
//PRECOND: LISTA NO VACIA
    NodoL* temp = xs->primero;
    xs->primero = xs->primero->siguiente;
    xs->cantidad--;
}

int length(LinkedList xs){
    return xs->cantidad;
}

void Snoc(int x, LinkedList xs){
    NodoL* nodo = new NodoL;
    nodo->elem = x;
    nodo->siguiente = NULL;
    if(xs->primero == NULL) {xs->primero = nodo;}
    else {
        NodoL* actual = xs->primero;
        while(actual->siguiente != NULL){
            actual = actual->siguiente;
        }
        actual->siguiente = nodo;
    }
    xs->cantidad++;
}

void DestroyL(LinkedList xs){
    NodoL* temp = xs->primero;
    while (xs->primero != NULL)
    {
        xs->primero = xs->primero->siguiente;
        delete temp;
        temp = xs->primero;
    }
    delete xs;    
}