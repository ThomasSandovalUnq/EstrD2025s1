#include <string>
using namespace std;
struct PersonaSt;

typedef PersonaSt* Persona;

Persona consPersona(string nombre, int edad);
string nombre(Persona p);
int edad(Persona p);
void Crecer(Persona p);
void CambioDeNombre(string nombre, Persona p);
bool esMayorQueLaOtra(Persona p1, Persona p2);
Persona laQueEsMayor(Persona p1, Persona p2);