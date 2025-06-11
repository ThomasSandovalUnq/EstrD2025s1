#include <iostream>
#include "Persona.h"
using namespace  std;

//int main(){
//    Persona p = consPersona("Joker", 16);
//    cout << "Nombre = " << nombre(p) << ", Edad = " << edad(p) << endl;
//}

/*int main(){
    Persona p = consPersona("Clark Kent", 25);
    CambioDeNombre("Superman", p);
    Crecer(p);
    cout << "Nombre = " << nombre(p) << ", Edad = " << edad(p) << endl;
}*/

int main(){
    Persona p1 = consPersona("Clark Kent", 25);
    Persona p2 = consPersona("Joker", 16);
    cout << "Es Joker Mayor a Clark? = " << esMayorQueLaOtra(p2, p1) << endl;
    cout << "El que es mayor es = " << nombre(laQueEsMayor(p1, p2)) <<  endl;
}