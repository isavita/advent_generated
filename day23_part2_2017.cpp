#include <iostream>
#include <fstream>

bool isPrime(int n) {
    for (int i = 2; i * i <= n; i++) {
        if (n % i == 0) {
            return false;
        }
    }
    return true;
}

int main() {
    std::ifstream input("input.txt");
    int b = 57 * 100 + 100000;
    int c = b + 17000;
    int h = 0;

    for (int x = b; x <= c; x += 17) {
        if (!isPrime(x)) {
            h++;
        }
    }

    std::cout << h << std::endl;

    return 0;
}