#include "ArrayList.h"
using namespace std;
#include <iostream>

int sumatoria(ArrayList xs){
    int contador = 0;
    for (int i=0; i<lengthAL(xs); i++){
        contador = contador + get(i, xs);
    }
    return contador;
}

//g++ -o app cpps 

/*int main(){
    ArrayList lNum = newArrayList();
    add(2,lNum);
    add(3,lNum);
    add(4,lNum);
    cout << sumatoria(lNum) << endl;
}*/

ArrayList sucesores(ArrayList xs){
    for (int i=0; i<lengthAL(xs); i++){
        set(i, (get(i, xs) + 1), xs);
    }
    return xs;
}

/*int main(){
    ArrayList lNum = newArrayList();
    add(78,lNum);
    add(46,lNum);
    add(74,lNum);
    ArrayList l = sucesores(lNum);
    cout << sumatoria(l) << endl;
}*/

bool pertenece(int x, ArrayList xs){
    bool res = false;
    for (int i = 0; i < lengthAL(xs); i++)
    {
        if(get(i, xs) == x){
            res = true;
            break;
        }
    }
    return res;
}

/*int main(){
    ArrayList lNum = newArrayList();
    add(78,lNum);
    add(46,lNum);
    add(74,lNum);
    cout << pertenece(74, lNum) << endl;
}*/

int apariciones(int x, ArrayList xs){
    int cont = 0;
    for (int i = 0; i < lengthAL(xs); i++)
    {
        if(get(i, xs) == x){
            cont++;
        }
    }
    return cont;
}

/*int main(){
    ArrayList lNum = newArrayList();
    add(78,lNum);
    add(46,lNum);
    add(74,lNum);
    add(10,lNum);
    add(10,lNum);
    add(10,lNum);
    cout << apariciones(10, lNum) << endl;
}*/

ArrayList append(ArrayList xs, ArrayList ys){
    ArrayList zs = newArrayListWith(lengthAL(xs)+lengthAL(ys));
    for (int i = 0; i < lengthAL(xs); i++)
    {
        add(get(i,xs), zs);
    }
    for (int i = 0; i < lengthAL(ys); i++)
    {
        add(get(i,ys), zs);
    }
    return zs;
}

/*int main(){
    ArrayList lNum1 = newArrayList();
    add(78,lNum1);
    add(46,lNum1);
    add(74,lNum1);
    ArrayList lNum2 = newArrayList();
    add(8,lNum2);
    add(6,lNum2);
    add(4,lNum2);
    ArrayList listaFinal = append(lNum1, lNum2);
    cout << sumatoria(listaFinal) << endl;
}*/

int minimo(ArrayList xs){
    if (lengthAL(xs)==0){
        return -1;
    }

    int min = get(0, xs);

    for (int i = 1; i < lengthAL(xs); i++)
    {
        if (min > get(i, xs))
        {
            min = get(i, xs);
        }
    }
    return min;
}

/*int main(){
    ArrayList lNum = newArrayList();
    cout << "Resultado: " << minimo(lNum) << endl;
}*/

int main(){
    ArrayList lNum = newArrayList();
    add(78,lNum);
    add(46,lNum);
    add(74,lNum);
    add(10,lNum);
    add(10,lNum);
    add(10,lNum);
    cout << "Resultado: " << minimo(lNum) << endl;
}