
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

static const uint32_t s[] = {
    7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22,
    5, 9, 14, 20, 5, 9, 14, 20, 5, 9, 14, 20, 5, 9, 14, 20,
    4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23,
    6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21
};
static const uint32_t K[] = {
    0xd76aa478, 0xe8c7b756, 0x242070db, 0xc1bdceee,
    0xf57c0faf, 0x4787c62a, 0xa8304613, 0xfd469501,
    0x698098d8, 0x8b44f7af, 0xffff5bb1, 0x895cd7be,
    0x6b901122, 0xfd987193, 0xa679438e, 0x49b40821,
    0xf61e2562, 0xc040b340, 0x265e5a51, 0xe9b6c7aa,
    0xd62f105d, 0x02441453, 0xd8a1e681, 0xe7d3fbc8,
    0x21e1cde6, 0xc33707d6, 0xf4d50d87, 0x455a14ed,
    0xa9e3e905, 0xfcefa3f8, 0x676f02d9, 0x8d2a4c8a,
    0xfffa3942, 0x8771f681, 0x6d9d6122, 0xfde5380c,
    0xa4beea44, 0x4bdecfa9, 0xf6bb4b60, 0xbebfbc70,
    0x289b7ec6, 0xeaa127fa, 0xd4ef3085, 0x04881d05,
    0xd9d4d039, 0xe6db99e5, 0x1fa27cf8, 0xc4ac5665,
    0xf4292244, 0x432aff97, 0xab9423a7, 0xfc93a039,
    0x655b59c3, 0x8f0ccc92, 0xffeff47d, 0x85845dd1,
    0x6fa87e4f, 0xfe2ce6e0, 0xa3014314, 0x4e0811a1,
    0xf7537e82, 0xbd3af235, 0x2ad7d2bb, 0xeb86d391
};

#define ROTL(x, c) (((x) << (c)) | ((x) >> (32 - (c))))

void md5_compute(const uint8_t *msg, size_t len, uint8_t digest[16]) {
    uint32_t h0 = 0x67452301, h1 = 0xEFCDAB89, h2 = 0x98BADCFE, h3 = 0x10325476;
    size_t padded_len = len + 1;
    while (padded_len % 64 != 56) padded_len++;
    padded_len += 8;

    uint8_t *padded_msg = calloc(padded_len, 1);
    memcpy(padded_msg, msg, len);
    padded_msg[len] = 0x80;
    uint64_t bit_len = len * 8;
    memcpy(padded_msg + padded_len - 8, &bit_len, 8);

    for (size_t i = 0; i < padded_len; i += 64) {
        uint32_t *M = (uint32_t *)(padded_msg + i);
        uint32_t A = h0, B = h1, C = h2, D = h3;
        for (int j = 0; j < 64; j++) {
            uint32_t F, g;
            if (j < 16) { F = (B & C) | (~B & D); g = j; }
            else if (j < 32) { F = (D & B) | (~D & C); g = (5 * j + 1) % 16; }
            else if (j < 48) { F = B ^ C ^ D; g = (3 * j + 5) % 16; }
            else { F = C ^ (B | ~D); g = (7 * j) % 16; }
            uint32_t temp = D;
            D = C; C = B;
            B = B + ROTL(A + F + K[j] + M[g], s[j]);
            A = temp;
        }
        h0 += A; h1 += B; h2 += C; h3 += D;
    }
    free(padded_msg);
    memcpy(digest, &h0, 4); memcpy(digest + 4, &h1, 4);
    memcpy(digest + 8, &h2, 4); memcpy(digest + 12, &h3, 4);
}

void to_hex(const uint8_t digest[16], char hex_str[33]) {
    for (int i = 0; i < 16; i++) {
        sprintf(hex_str + i * 2, "%02x", digest[i]);
    }
}

#define CACHE_SIZE 40000
char *g_cache[CACHE_SIZE] = {NULL};

char* get_stretched_hash(const char *salt, int idx) {
    if (idx < 0 || idx >= CACHE_SIZE) return NULL;
    if (g_cache[idx] != NULL) return g_cache[idx];

    char msg_buf[128];
    uint8_t digest[16];
    char hex_str[33];
    
    sprintf(msg_buf, "%s%d", salt, idx);
    md5_compute((uint8_t*)msg_buf, strlen(msg_buf), digest);
    to_hex(digest, hex_str);

    for (int i = 0; i < 2016; i++) {
        md5_compute((uint8_t*)hex_str, 32, digest);
        to_hex(digest, hex_str);
    }
    
    g_cache[idx] = strdup(hex_str);
    return g_cache[idx];
}

int main(void) {
    FILE *f = fopen("input.txt", "r");
    char salt[64];
    fgets(salt, sizeof(salt), f);
    fclose(f);
    salt[strcspn(salt, "\r\n")] = 0;

    int found_keys = 0;
    int index = 0;
    while (found_keys < 64) {
        const char *hash = get_stretched_hash(salt, index);
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
                if (strstr(get_stretched_hash(salt, index + j), quintuplet)) {
                    found_keys++;
                    break;
                }
            }
        }
        
        if (found_keys == 64) {
            printf("%d\n", index);
            break;
        }
        index++;
    }

    for (int i = 0; i < CACHE_SIZE; i++) {
        if (g_cache[i]) free(g_cache[i]);
    }

    return 0;
}
