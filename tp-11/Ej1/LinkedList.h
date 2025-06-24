#include <string>
using namespace std;

struct LinkedListSt;
typedef LinkedListSt* LinkedList; // INV.REP.: el puntero NO es NULL

LinkedList nil();
//Crea una lista vacía
bool isEmpty(LinkedList xs);
//Indica si la lista está vacía.
int head(LinkedList xs);
//Devuelve el primer elemento.
void Cons(int x, LinkedList xs);
//Agrega un elemento al principio de la lista.
void Tail(LinkedList xs);
//Quita el primer elemento.
int length(LinkedList xs);
//Devuelve la cantidad de elementos.
void Snoc(int x, LinkedList xs);
//Agrega un elemento al final de la lista.
void DestroyL(LinkedList xs);
//Libera la memoria ocupada por la lista