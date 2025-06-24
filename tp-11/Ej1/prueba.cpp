#include "Iterator.h"
#include <iostream>
using namespace std;

int main() {
    LinkedList xs = new LinkedListSt{0, NULL};
    
    Snoc(10, xs);
    Cons(5, xs);
    Snoc(15, xs);

    ListIterator ixs = new IteratorSt;
    ixs = getIterator(xs);

    while (!atEnd(ixs)) {
        cout << current(ixs) << " ";
        Next(ixs);
    }

    cout << endl;
    
    DisposeIterator(ixs);
    cout << "Tamanio de la linked list = " << length(xs) << endl;
    cout << "Esta vacia? " << isEmpty(xs) << endl;
    cout << "El 1ero es " << head(xs) << endl;
    DestroyL(xs);
}