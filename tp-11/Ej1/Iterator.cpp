#include "Iterator.h"
#include <string>
using namespace std;

ListIterator getIterator(LinkedList xs){
    IteratorSt* ixs = new IteratorSt;
    ixs->current = xs->primero;
    return ixs;
}

int current(ListIterator ixs){
    return ixs->current->elem;
}

void SetCurrent(int x, ListIterator ixs){
    ixs->current->elem = x;
}

void Next(ListIterator ixs){
    ixs->current = ixs->current->siguiente;
}

bool atEnd(ListIterator ixs){
    return ixs->current == NULL;
}

void DisposeIterator(ListIterator ixs){
    delete ixs;
}
