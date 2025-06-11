#include "TipoDePokemon.h"
#include "Pokemon.h"

struct PokeSt {
TipoDePokemon tipo;
int vida;
};

Pokemon consPokemon(TipoDePokemon tipo){
    PokeSt* p = new PokeSt;
    p->tipo = tipo;
    p->vida = 100;
    return p;
}

TipoDePokemon tipoDePokemon(Pokemon p){
    return p->tipo;
}

int energia(Pokemon p){
    return p->vida;
}

void PerderEnergia(int energia, Pokemon p){
    p->vida = p->vida - energia;
}

bool superaA(Pokemon p1, Pokemon p2){
    if (p1->tipo == "agua" && p2->tipo == "fuego") {
        return true;
    }
    if (p1->tipo == "fuego" && p2->tipo == "planta") {
        return true;
    }
    if (p1->tipo == "planta" && p2->tipo == "agua") {
        return true;
    }
    return false;
}