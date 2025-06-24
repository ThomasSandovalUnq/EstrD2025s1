#include "Iterator.h"
#include "NodoL.h"
#include "LinkedList.h"
#include <string>
using namespace std;

struct IteratorSt {
NodoL* current;
};

ListIterator getIterator(LinkedList xs){
    
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