
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

// Use unsigned 64-bit integers for large numbers
typedef unsigned long long u64;

// Constants from the problem
const u64 SIZE = 119315717514047ULL;
const u64 ITERATIONS = 101741582076661ULL;
const u64 TARGET_POS = 2020ULL;

// Modular addition: (a + b) % m
u64 mod_add(u64 a, u64 b, u64 m) {
    a %= m;
    b %= m;
    u64 res = a + b;
    if (res < a || res >= m) { // Handle potential overflow and ensure result < m
        res -= m;
    }
    return res;
}

// Modular subtraction: (a - b) % m
u64 mod_sub(u64 a, u64 b, u64 m) {
    a %= m;
    b %= m;
    // Add m before subtracting to ensure non-negative result before final modulo
    return (a + m - b) % m;
}

// Modular multiplication: (a * b) % m
// Uses __int128 for intermediate product if available (GCC/Clang)
// Otherwise, falls back to a slower but portable method.
u64 mod_mul(u64 a, u64 b, u64 m) {
#ifdef __SIZEOF_INT128__
    unsigned __int128 res = (unsigned __int128)a * b;
    return (u64)(res % m);
#else
    // Portable binary multiplication (handles potential overflow)
    u64 res = 0;
    a %= m;
    while (b > 0) {
        if (b & 1) {
            res = mod_add(res, a, m);
        }
        a = mod_add(a, a, m); // Double a
        b >>= 1;             // Halve b
    }
    return res;
#endif
}


// Modular exponentiation: (base^exp) % mod
u64 mod_pow(u64 base, u64 exp, u64 mod) {
    u64 res = 1;
    base %= mod;
    while (exp > 0) {
        if (exp % 2 == 1) res = mod_mul(res, base, mod);
        base = mod_mul(base, base, mod);
        exp /= 2;
    }
    return res;
}

// Modular multiplicative inverse: n^-1 % mod
// Using Fermat's Little Theorem (mod must be prime)
u64 mod_inverse(u64 n, u64 mod) {
    // We know SIZE is prime, so mod-2 is valid exponent
    return mod_pow(n, mod - 2, mod);
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening input.txt");
        return 1;
    }

    u64 offset = 0;
    u64 increment = 1;
    char line[100];

    while (fgets(line, sizeof(line), file)) {
        // Remove trailing newline if present
        line[strcspn(line, "\n")] = 0;

        if (strcmp(line, "deal into new stack") == 0) {
            increment = mod_mul(increment, SIZE - 1, SIZE); // Multiply by -1 (mod SIZE)
            offset = mod_add(offset, increment, SIZE);
        } else if (strncmp(line, "cut", 3) == 0) {
            long long n_signed;
            sscanf(line, "cut %lld", &n_signed);
            u64 n = (n_signed >= 0) ? (u64)n_signed : (SIZE - (u64)(-n_signed)); // Handle negative cut
            n %= SIZE;
             offset = mod_add(offset, mod_mul(n, increment, SIZE), SIZE);
        } else if (strncmp(line, "deal with increment", 19) == 0) {
            u64 n;
            sscanf(line, "deal with increment %llu", &n);
            u64 inv = mod_inverse(n, SIZE);
            increment = mod_mul(increment, inv, SIZE);
        }
    }
    fclose(file);

    // Calculate the effect of applying the shuffle ITERATIONS times
    // Let one shuffle be f(x) = (ax + b) % SIZE. Here a=increment, b=offset
    // Applying it k times: f^k(x) = (a^k * x + b * (a^k - 1) / (a - 1)) % SIZE

    // final_increment = increment^ITERATIONS % SIZE
    u64 final_increment = mod_pow(increment, ITERATIONS, SIZE);

    // final_offset = offset * (final_increment - 1) * mod_inverse(increment - 1, SIZE) % SIZE
    u64 term1 = mod_sub(final_increment, 1, SIZE); // (increment^ITERATIONS - 1)
    u64 term2_inv = mod_inverse(mod_sub(increment, 1, SIZE), SIZE); // (increment - 1)^-1
    u64 final_offset = mod_mul(offset, term1, SIZE);
    final_offset = mod_mul(final_offset, term2_inv, SIZE);

    // Find the value at TARGET_POS (2020)
    // value = (TARGET_POS * final_increment + final_offset) % SIZE
    u64 target_val_times_incr = mod_mul(TARGET_POS, final_increment, SIZE);
    u64 answer = mod_add(target_val_times_incr, final_offset, SIZE);

    printf("%llu\n", answer);

    return 0;
}
