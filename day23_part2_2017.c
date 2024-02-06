
#include <stdio.h>

int isPrime(int n) {
    for (int i = 2; i*i <= n; i++) {
        if (n % i == 0) {
            return 0;
        }
    }
    return 1;
}

int main() {
    int b = 57*100 + 100000; // Initial value computed for register b
    int c = b + 17000;       // Initial value computed for register c
    int h = 0;               // Initialize register h

    for (int x = b; x <= c; x += 17) { // Simulate the loop from b to c with step 17
        if (!isPrime(x)) {
            h++;
        }
    }

    printf("%d\n", h);

    return 0;
}
