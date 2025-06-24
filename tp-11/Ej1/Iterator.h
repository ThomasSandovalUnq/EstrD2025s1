#include <string>
#include "LinkedList.h"
using namespace std;

struct IteratorSt {
NodoL* current;
};
typedef IteratorSt* ListIterator; // INV.REP.: el puntero NO es NULL

ListIterator getIterator(LinkedList xs);
//Apunta el recorrido al primer elemento.
int current(ListIterator ixs);
//Devuelve el elemento actual en el recorrido.
void SetCurrent(int x, ListIterator ixs);
//Reemplaza el elemento actual por otro elemento.
void Next(ListIterator ixs);
//Pasa al siguiente elemento.
bool atEnd(ListIterator ixs);
//Indica si el recorrido ha terminado.
void DisposeIterator(ListIterator ixs);
//Libera la memoria ocupada por el iterador.