#include <iostream>

using namespace std;

extern "C" {
int simple_arith();
}

int main() { cout << "calling four: " << simple_arith() << endl; }