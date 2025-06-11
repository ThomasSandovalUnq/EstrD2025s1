#include <string>
#include "TipoDePokemon.h"
using namespace std;

struct PokeSt;
typedef PokeSt* Pokemon;

Pokemon consPokemon(TipoDePokemon tipo);
TipoDePokemon tipoDePokemon(Pokemon p);
int energia(Pokemon p);
void PerderEnergia(int energia, Pokemon p);
bool superaA(Pokemon p1, Pokemon p2);