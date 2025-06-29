#include "Iterator.h"
#include <string>
using namespace std;

ListIterator getIterator(LinkedList xs){           //O(1)
    IteratorSt* ixs = new IteratorSt;
    ixs->current = xs->primero;
    return ixs;
}

int current(ListIterator ixs){           //O(1)
    return ixs->current->elem;
}

void SetCurrent(int x, ListIterator ixs){           //O(1)
    ixs->current->elem = x;
}

void Next(ListIterator ixs){           //O(1)
    ixs->current = ixs->current->siguiente;
}

bool atEnd(ListIterator ixs){           //O(1)
    return ixs->current == NULL;
}

void DisposeIterator(ListIterator ixs){           //O(1)
    delete ixs;
}
