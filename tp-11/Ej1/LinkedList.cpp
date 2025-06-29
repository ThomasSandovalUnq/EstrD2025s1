#include <string>
#include "LinkedList.h"
using namespace std;

LinkedList nil(){
    LinkedListSt* l = new LinkedListSt;
    l->cantidad = 0;
    l->primero = NULL;
    l->ultimo = NULL;
    return l;
}

bool isEmpty(LinkedList xs){           //O(1)
    return xs->cantidad == 0;
}

int head(LinkedList xs){           //O(1)
//PRECOND: LA LISTA NO PUEDE SER VACIA
    return (xs->primero->elem);
}

void Cons(int x, LinkedList xs){           //O(1)
    NodoL* nodo = new NodoL;
    nodo->elem = x;
    nodo->siguiente = xs->primero;
    xs->primero = nodo;
    xs->cantidad++;
}

void Tail(LinkedList xs){           //O(1)
//PRECOND: LISTA NO VACIA
    NodoL* temp = xs->primero;
    xs->primero = xs->primero->siguiente;
    xs->cantidad--;
    delete temp;
}

int length(LinkedList xs){           //O(1)
    return xs->cantidad;
}

void Snoc(int x, LinkedList xs){    //O(1)
    NodoL* nodo = new NodoL;
    nodo->elem = x;
    nodo->siguiente = NULL;

    if (xs->primero == NULL) {
        xs->primero = nodo;
    } else {
        xs->ultimo->siguiente = nodo;
    }
    xs->ultimo = nodo;
    xs->cantidad++;
}

/*void DestroyL(LinkedList xs){           
    NodoL* temp = xs->primero;
    while (xs->primero != NULL)
    {
        xs->primero = xs->primero->siguiente;
        delete temp;
        temp = xs->primero;
    }
    delete xs;    
}*/

void DestroyL(LinkedList xs){       //O(1)
    delete xs;
}

void Append(LinkedList xs, LinkedList ys){  //O(1)
    if (xs->primero == NULL) { 
        xs->primero = ys->primero;
        xs->ultimo = ys->ultimo; 
    }
        else {
            xs->ultimo->siguiente = ys->primero; 
            xs->ultimo = ys->ultimo;
        }
    xs->cantidad = xs->cantidad + ys->cantidad;
    delete ys;
}