#include "Iterator.h"
#include <string>
using namespace std;

ListIterator getIterator(LinkedList xs){
    IteratorSt* ixs = new IteratorSt;
    ixs->current = xs->primero;
    return ixs;
}

int current(ListIterator ixs);

void SetCurrent(int x, ListIterator ixs);
//Reemplaza el elemento actual por otro elemento.
void Next(ListIterator ixs);
//Pasa al siguiente elemento.
bool atEnd(ListIterator ixs);
//Indica si el recorrido ha terminado.
void DisposeIterator(ListIterator ixs);
//Libera la memoria ocupada por el iterador.