#include "Iterator.h"
#include <iostream>
using namespace std;

int main() {
    LinkedList xs = new LinkedListSt{0, NULL};
    
    Snoc(10, xs);
    Cons(5, xs);
    Snoc(15, xs);

    DestroyL(xs);

    cout << head(xs) << endl;
}