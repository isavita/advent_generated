
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <ctype.h>

static const uint32_t k[] = {
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

static const uint32_t r[] = {
    7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22,
    5, 9, 14, 20, 5, 9, 14, 20, 5, 9, 14, 20, 5, 9, 14, 20,
    4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23,
    6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21};

#define LROTATE(x, c) (((x) << (c)) | ((x) >> (32 - (c))))

void compute_md5(const uint8_t *msg_in, size_t len_in, uint32_t *hash_out) {
    hash_out[0] = 0x67452301;
    hash_out[1] = 0xEFCDAB89;
    hash_out[2] = 0x98BADCFE;
    hash_out[3] = 0x10325476;

    size_t padded_len = (((len_in + 8) / 64) + 1) * 64;
    uint8_t *msg = (uint8_t *)calloc(padded_len, 1);
    memcpy(msg, msg_in, len_in);
    msg[len_in] = 0x80;
    uint64_t bit_len = len_in * 8;
    memcpy(msg + padded_len - 8, &bit_len, 8);

    for (size_t offset = 0; offset < padded_len; offset += 64) {
        uint32_t *w = (uint32_t *)(msg + offset);
        uint32_t a = hash_out[0], b = hash_out[1], c = hash_out[2], d = hash_out[3];
        for (int i = 0; i < 64; i++) {
            uint32_t f, g;
            if (i <= 15) {
                f = (b & c) | (~b & d);
                g = i;
            } else if (i <= 31) {
                f = (d & b) | (~d & c);
                g = (5 * i + 1) % 16;
            } else if (i <= 47) {
                f = b ^ c ^ d;
                g = (3 * i + 5) % 16;
            } else {
                f = c ^ (b | ~d);
                g = (7 * i) % 16;
            }
            uint32_t temp = d;
            d = c;
            c = b;
            b = b + LROTATE(a + f + k[i] + w[g], r[i]);
            a = temp;
        }
        hash_out[0] += a;
        hash_out[1] += b;
        hash_out[2] += c;
        hash_out[3] += d;
    }
    free(msg);
}

int main(void) {
    char key[64];
    FILE *file = fopen("input.txt", "r");
    if (!file || !fgets(key, sizeof(key), file)) return 1;
    fclose(file);
    
    size_t len = strlen(key);
    while (len > 0 && isspace((unsigned char)key[len - 1])) {
        key[--len] = '\0';
    }

    char buffer[128];
    uint32_t hash[4];
    long number = 0;

    while (1) {
        int total_len = sprintf(buffer, "%s%ld", key, number);
        compute_md5((uint8_t*)buffer, total_len, hash);
        if ((hash[0] & 0x00F0FFFF) == 0) {
            printf("%ld\n", number);
            break;
        }
        number++;
    }
    
    return 0;
}
