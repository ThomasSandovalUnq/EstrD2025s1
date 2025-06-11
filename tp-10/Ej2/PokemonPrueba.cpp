#include <iostream>
#include "Pokemon.h"
#include "Entrenador.h"

int main() {
    Pokemon charizard = consPokemon("fuego");
    Pokemon blastoise = consPokemon("agua");
    Pokemon venusaur = consPokemon("planta");
    Pokemon pikachu = consPokemon("fuego");

    Pokemon pokes1[2];
    pokes1[0] = venusaur;
    pokes1[1] = charizard;

    Entrenador ash = consEntrenador("Ash", 2, pokes1);

    Pokemon pokes2[1];
    pokes2[0] = blastoise;
    pokes2[1] = blastoise;

    Entrenador misty = consEntrenador("Misty", 2, pokes2);

    std::cout << "Nombre de Entrenador 1: " << nombreDeEntrenador(ash) << std::endl;
    std::cout << "Cantidad de Pokemon de Ash: " << cantidadDePokemon(ash) << std::endl;
    std::cout << "Cantidad de Pokemon de Fuego de Ash: " << cantidadDePokemonDe("fuego", ash) << std::endl;
    std::cout << "Pokemon Nro 0 de Ash (tipo): " << tipoDePokemon(pokemonNro(0, ash)) << std::endl;

    std::cout << "\nNombre de Entrenador 2: " << nombreDeEntrenador(misty) << std::endl;
    std::cout << "Cantidad de Pokemon de Misty: " << cantidadDePokemon(misty) << std::endl;
    std::cout << "Cantidad de Pokemon de Agua de Misty: " << cantidadDePokemonDe("agua", misty) << std::endl;
    std::cout << "Pokemon Nro 1 de Misty (tipo): " << tipoDePokemon(pokemonNro(1, misty)) << std::endl;

    std::cout << "\nAsh le gana a todos los Pokemon de Misty? ";
    if (leGanaATodos(ash, misty)) {
        std::cout << "Si" << std::endl;
    } else {
        std::cout << "No" << std::endl;
    }
}