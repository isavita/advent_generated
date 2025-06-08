
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

#define CACHE_SIZE 2048
#define ROTL(x, c) (((x) << (c)) | ((x) >> (32 - (c))))

static const uint8_t S[] = {
    7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22,
    5, 9, 14, 20, 5, 9, 14, 20, 5, 9, 14, 20, 5, 9, 14, 20,
    4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23,
    6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21
};

static const uint32_t K[] = {
    0xd76aa478, 0xe8c7b756, 0x242070db, 0xc1bdceee, 0xf57c0faf, 0x4787c62a,
    0xa8304613, 0xfd469501, 0x698098d8, 0x8b44f7af, 0xffff5bb1, 0x895cd7be,
    0x6b901122, 0xfd987193, 0xa679438e, 0x49b40821, 0xf61e2562, 0xc040b340,
    0x265e5a51, 0xe9b6c7aa, 0xd62f105d, 0x02441453, 0xd8a1e681, 0xe7d3fbc8,
    0x21e1cde6, 0xc33707d6, 0xf4d50d87, 0x455a14ed, 0xa9e3e905, 0xfcefa3f8,
    0x676f02d9, 0x8d2a4c8a, 0xfffa3942, 0x8771f681, 0x6d9d6122, 0xfde5380c,
    0xa4beea44, 0x4bdecfa9, 0xf6bb4b60, 0xbebfbc70, 0x289b7ec6, 0xeaa127fa,
    0xd4ef3085, 0x04881d05, 0xd9d4d039, 0xe6db99e5, 0x1fa27cf8, 0xc4ac5665,
    0xf4292244, 0x432aff97, 0xab9423a7, 0xfc93a039, 0x655b59c3, 0x8f0ccc92,
    0xffeff47d, 0x85845dd1, 0x6fa87e4f, 0xfe2ce6e0, 0xa3014314, 0x4e0811a1,
    0xf7537e82, 0xbd3af235, 0x2ad7d2bb, 0xeb86d391
};

typedef struct {
    int index;
    char hash[33];
} CacheEntry;

CacheEntry cache[CACHE_SIZE];

void to_hex(const uint8_t* digest, char* hex_out) {
    for (int i = 0; i < 16; i++) {
        sprintf(hex_out + i * 2, "%02x", digest[i]);
    }
}

void md5(const uint8_t* msg, size_t len, uint8_t* digest) {
    uint32_t h0 = 0x67452301, h1 = 0xEFCDAB89, h2 = 0x98BADCFE, h3 = 0x10325476;

    uint64_t bit_len = len * 8;
    size_t new_len = ((len + 8) / 64 + 1) * 64;
    uint8_t* padded_msg = (uint8_t*)calloc(new_len, 1);
    memcpy(padded_msg, msg, len);
    padded_msg[len] = 0x80;
    memcpy(padded_msg + new_len - 8, &bit_len, 8);

    for (size_t offset = 0; offset < new_len; offset += 64) {
        uint32_t* M = (uint32_t*)(padded_msg + offset);
        uint32_t A = h0, B = h1, C = h2, D = h3;

        for (int i = 0; i < 64; i++) {
            uint32_t F, g;
            if (i < 16) {
                F = (B & C) | ((~B) & D); g = i;
            } else if (i < 32) {
                F = (D & B) | ((~D) & C); g = (5 * i + 1) % 16;
            } else if (i < 48) {
                F = B ^ C ^ D; g = (3 * i + 5) % 16;
            } else {
                F = C ^ (B | (~D)); g = (7 * i) % 16;
            }
            uint32_t temp = D;
            D = C;
            C = B;
            B = B + ROTL(A + F + K[i] + M[g], S[i]);
            A = temp;
        }
        h0 += A; h1 += B; h2 += C; h3 += D;
    }
    free(padded_msg);
    memcpy(digest, &h0, 4);
    memcpy(digest + 4, &h1, 4);
    memcpy(digest + 8, &h2, 4);
    memcpy(digest + 12, &h3, 4);
}

const char* get_hash(const char* salt, int index) {
    int cache_idx = index % CACHE_SIZE;
    if (cache[cache_idx].index == index) {
        return cache[cache_idx].hash;
    }

    char buffer[256];
    int len = sprintf(buffer, "%s%d", salt, index);
    uint8_t digest[16];
    md5((uint8_t*)buffer, len, digest);

    cache[cache_idx].index = index;
    to_hex(digest, cache[cache_idx].hash);
    return cache[cache_idx].hash;
}

int main() {
    FILE* f = fopen("input.txt", "r");
    char salt[100];
    fscanf(f, "%s", salt);
    fclose(f);

    for (int i = 0; i < CACHE_SIZE; i++) cache[i].index = -1;

    int found_keys = 0;
    int index = 0;
    while (found_keys < 64) {
        const char* hash = get_hash(salt, index);
        char triplet_char = 0;
        for (int i = 0; i < 30; i++) {
            if (hash[i] == hash[i+1] && hash[i] == hash[i+2]) {
                triplet_char = hash[i];
                break;
            }
        }

        if (triplet_char) {
            char quintuplet[6];
            memset(quintuplet, triplet_char, 5);
            quintuplet[5] = '\0';
            for (int j = 1; j <= 1000; j++) {
                if (strstr(get_hash(salt, index + j), quintuplet)) {
                    found_keys++;
                    break;
                }
            }
        }
        index++;
    }

    printf("%d\n", index - 1);

    return 0;
}
