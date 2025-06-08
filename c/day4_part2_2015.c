
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

#define LEFT_ROTATE(x, c) (((x) << (c)) | ((x) >> (32 - (c))))

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
    0xf7537e82, 0xbd3af235, 0x2ad7d2bb, 0xeb86d391};

static const uint32_t R[] = {
    7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22,
    5, 9, 14, 20, 5, 9, 14, 20, 5, 9, 14, 20, 5, 9, 14, 20,
    4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23,
    6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21};

void md5_transform(uint32_t state[4], const uint32_t block[16]) {
    uint32_t a = state[0], b = state[1], c = state[2], d = state[3];

    for (int i = 0; i < 64; ++i) {
        uint32_t f, g;
        if (i < 16) {
            f = (b & c) | (~b & d);
            g = i;
        } else if (i < 32) {
            f = (d & b) | (~d & c);
            g = (5 * i + 1) % 16;
        } else if (i < 48) {
            f = b ^ c ^ d;
            g = (3 * i + 5) % 16;
        } else {
            f = c ^ (b | ~d);
            g = (7 * i) % 16;
        }
        uint32_t temp = d;
        d = c;
        c = b;
        b = b + LEFT_ROTATE(a + f + K[i] + block[g], R[i]);
        a = temp;
    }
    state[0] += a;
    state[1] += b;
    state[2] += c;
    state[3] += d;
}

void compute_md5(const char* msg, size_t len, uint8_t digest[16]) {
    uint32_t h[4] = {0x67452301, 0xEFCDAB89, 0x98BADCFE, 0x10325476};
    
    size_t num_blocks = ((len + 8) / 64) + 1;
    size_t padded_size = num_blocks * 64;

    uint8_t* padded_msg = calloc(padded_size, 1);
    if (!padded_msg) exit(1);

    memcpy(padded_msg, msg, len);
    padded_msg[len] = 0x80;
    
    uint64_t bit_len = len * 8;
    memcpy(padded_msg + padded_size - 8, &bit_len, 8);
    
    for (size_t i = 0; i < padded_size; i += 64) {
        md5_transform(h, (uint32_t*)(padded_msg + i));
    }
    
    free(padded_msg);
    memcpy(digest, h, 16);
}

long find_advent_coin(const char* secret_key) {
    long number = 0;
    size_t key_len = strlen(secret_key);
    char buffer[128];
    memcpy(buffer, secret_key, key_len);

    while (1) {
        int num_len = sprintf(buffer + key_len, "%ld", number);
        uint8_t digest[16];
        compute_md5(buffer, key_len + num_len, digest);
        if (digest[0] == 0 && digest[1] == 0 && digest[2] == 0) {
            return number;
        }
        number++;
    }
}

int main(void) {
    FILE* fp = fopen("input.txt", "r");
    if (!fp) return 1;

    char key[64];
    if (!fgets(key, sizeof(key), fp)) {
        fclose(fp);
        return 1;
    }
    fclose(fp);
    
    key[strcspn(key, "\r\n")] = 0;
    
    printf("%ld\n", find_advent_coin(key));
    
    return 0;
}
