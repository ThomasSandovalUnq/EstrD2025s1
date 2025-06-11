#include <string>
#include "TipoDePokemon.h"
#include "Pokemon.h"
#include "Entrenador.h"
using namespace std;

struct EntrenadorSt
{
    string nombre;
    Pokemon* pokemon;
    int cantPokemon;
};

Entrenador consEntrenador(string nombre, int cantidad, Pokemon* pokemon){
    EntrenadorSt* e = new EntrenadorSt;
    e->nombre = nombre;
    e->cantPokemon = cantidad;
    e->pokemon = new Pokemon[cantidad];
    for (int i = 0; i < cantidad; ++i) {
        e->pokemon[i] = pokemon[i];
    }
    return e;
}

string nombreDeEntrenador(Entrenador e){
    return e->nombre;
}

int cantidadDePokemon(Entrenador e){
    return e->cantPokemon;
}

int cantidadDePokemonDe(TipoDePokemon tipo, Entrenador e){
    int count = 0;
    for(int i = 0; i < e->cantPokemon; i++){
        if (tipoDePokemon(e->pokemon[i]) == tipo){
            count++;
        }
    }
    return count;
}

Pokemon pokemonNro(int i, Entrenador e){
    //Precondición: existen al menos i − 1 pokémon.
    return e->pokemon[i];
}

bool leGanaATodos(Entrenador e1, Entrenador e2){
    for (int i = 0; i < e2->cantPokemon; ++i) {
        Pokemon poke2 = e2->pokemon[i];
        bool foundWinningPokemon = false;
        for (int j = 0; j < e1->cantPokemon; ++j) {
            Pokemon poke1 = e1->pokemon[j];
            if (superaA(poke1, poke2)) {
                foundWinningPokemon = true;
                break;
            }
        }
        if (!foundWinningPokemon) {
            return false;
        }
    }
    return true;
}