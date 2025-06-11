#include <iostream>
#include "Persona.h"

struct PersonaSt {
    string nombre;
    int edad;
};

Persona consPersona(string nombre, int edad){
    PersonaSt* p = new PersonaSt;
    p -> edad = edad;
    p -> nombre = nombre;
    return p;
}

string nombre(Persona p){
    return p->nombre;
}

int edad(Persona p){
    return p->edad;
}

void Crecer(Persona p){
    p->edad = p->edad + 1;
}

void CambioDeNombre(string nombre, Persona p){
    p->nombre = nombre;
}

bool esMayorQueLaOtra(Persona p1, Persona p2){
    return p1->edad > p2->edad;
}

Persona laQueEsMayor(Persona p1, Persona p2){
    if (p1->edad >= p2->edad){
        return p1;
    };
    return p2;
}