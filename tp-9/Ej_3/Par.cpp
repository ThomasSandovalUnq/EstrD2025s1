#include "Par.h"

Par consPar(int x, int y){
    Par p;
    p.x = x;
    p.y = y;
    return p;
}

int fst(Par p){
    return p.x;
}

int snd(Par p){
    return p.y;
}

int maxDelPar(Par p){
    if (p.x > p.y) {
        return p.x;
    };
    return p.y;
}

Par swap(Par p){
    return consPar(p.y,p.x);
}

Par divisionYResto(int m, int n){
    return consPar(m / n, m % n);
}