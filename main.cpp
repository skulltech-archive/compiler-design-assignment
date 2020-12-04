#include <iostream>

using namespace std;

extern "C" {
int main1();
}

int main() { cout << "calling main " << main1() << endl; }