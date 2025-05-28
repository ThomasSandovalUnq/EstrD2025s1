#include <iostream>
using namespace std;

//1 para true
//2 para false

bool pertenece(char c, string s){
    for (int i = 0; i < s.size(); i++) {
        if (s[i] == c) {
            return true;
        }
    }
    return false;
}

bool perteneceR(char c, string s) {
    if (s.empty()) return false;            
    if (s[0] == c) return true;             
    return perteneceR(c, s.substr(1));       
}

int main(){
    cout << "Pertenece: "<< pertenece('C', "Carlitos") << endl;
    cout << "Pertenece: "<< perteneceR('C', "Carlitos") << endl;
}