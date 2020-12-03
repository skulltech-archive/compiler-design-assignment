#include <iostream>

using namespace std;

extern "C" {
int four();
}

int main() { cout << "calling four: " << four() << endl; }