/*  Advent of Code 2016 – Day 5, Part 1
 *  Compile:  gcc -std=c99 -O2 -o day5 day5.c
 *  Usage:    ./day5          (expects Door ID in input.txt)
 *
 *  No external libraries – MD5 included below (public-domain implementation).
 */
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

/* ---------- MD5 implementation (public domain) ---------- */
#define LEFTROTATE(x,c) (((x) << (c)) | ((x) >> (32 - (c))))

static void md5(const uint8_t *initial_msg, size_t len, uint8_t *digest /*16 B*/)
{
    /* init constants per RFC 1321 */
    uint32_t h0 = 0x67452301u;
    uint32_t h1 = 0xefcdab89u;
    uint32_t h2 = 0x98badcfeu;
    uint32_t h3 = 0x10325476u;

    /* r specifies the per-round shift amounts */
    static const uint32_t r[] = {
         7,12,17,22,  7,12,17,22,  7,12,17,22,  7,12,17,22,
         5, 9,14,20,  5, 9,14,20,  5, 9,14,20,  5, 9,14,20,
         4,11,16,23,  4,11,16,23,  4,11,16,23,  4,11,16,23,
         6,10,15,21,  6,10,15,21,  6,10,15,21,  6,10,15,21
    };
    /* Use binary integer part of sines of integers (Radians) as constants */
    static const uint32_t k[] = {
        0xd76aa478u,0xe8c7b756u,0x242070dbu,0xc1bdceeeu,0xf57c0fafu,0x4787c62au,0xa8304613u,0xfd469501u,
        0x698098d8u,0x8b44f7afu,0xffff5bb1u,0x895cd7beu,0x6b901122u,0xfd987193u,0xa679438eu,0x49b40821u,
        0xf61e2562u,0xc040b340u,0x265e5a51u,0xe9b6c7aau,0xd62f105du,0x02441453u,0xd8a1e681u,0xe7d3fbc8u,
        0x21e1cde6u,0xc33707d6u,0xf4d50d87u,0x455a14edu,0xa9e3e905u,0xfcefa3f8u,0x676f02d9u,0x8d2a4c8au,
        0xfffa3942u,0x8771f681u,0x6d9d6122u,0xfde5380cu,0xa4beea44u,0x4bdecfa9u,0xf6bb4b60u,0xbebfbc70u,
        0x289b7ec6u,0xeaa127fau,0xd4ef3085u,0x04881d05u,0xd9d4d039u,0xe6db99e5u,0x1fa27cf8u,0xc4ac5665u,
        0xf4292244u,0x432aff97u,0xab9423a7u,0xfc93a039u,0x655b59c3u,0x8f0ccc92u,0xffeff47du,0x85845dd1u,
        0x6fa87e4fu,0xfe2ce6e0u,0xa3014314u,0x4e0811a1u,0xf7537e82u,0xbd3af235u,0x2ad7d2bbu,0xeb86d391u
    };

    /* preprocessing: padding with a single 1-bit then zeros, length encoded on 64 bits LE */
    size_t new_len = len + 1;
    while (new_len % 64 != 56) new_len++;
    uint8_t *msg = calloc(new_len + 8, 1);    /* zero-filled */
    memcpy(msg, initial_msg, len);
    msg[len] = 0x80;                          /* append the 1-bit */

    uint64_t bits_len = (uint64_t)len * 8;
    memcpy(msg + new_len, &bits_len, 8);      /* append original length in bits (little-endian) */

    /* process the message in successive 512-bit chunks */
    for (size_t offset = 0; offset < new_len; offset += 64) {
        uint32_t *w = (uint32_t *)(msg + offset);   /* 16 × 32-bit words (little-endian) */

        uint32_t a = h0, b = h1, c = h2, d = h3;

        for (uint32_t i = 0; i < 64; ++i) {
            uint32_t f, g;
            if (i < 16) {              f = (b & c) | (~b & d);        g = i;          }
            else if (i < 32) {         f = (d & b) | (~d & c);        g = (5*i + 1)%16; }
            else if (i < 48) {         f = b ^ c ^ d;                 g = (3*i + 5)%16; }
            else {                     f = c ^ (b | ~d);              g = (7*i) %16; }

            uint32_t temp = d;
            d = c;
            c = b;
            b = b + LEFTROTATE(a + f + k[i] + w[g], r[i]);
            a = temp;
        }

        h0 += a; h1 += b; h2 += c; h3 += d;
    }

    free(msg);

    /* output digest (little-endian) */
    memcpy(digest,     &h0, 4);
    memcpy(digest + 4, &h1, 4);
    memcpy(digest + 8, &h2, 4);
    memcpy(digest +12, &h3, 4);
}
/* ---------- end MD5 ---------- */

/* convert a nibble (0-15) to hex char */
static inline char hex_digit(uint8_t v) { return v < 10 ? '0' + v : 'a' + (v - 10); }

int main(void)
{
    /* --- read Door ID --- */
    FILE *fp = fopen("input.txt", "r");
    if (!fp) { perror("input.txt"); return EXIT_FAILURE; }

    char door_id[128];
    if (!fgets(door_id, sizeof door_id, fp)) { fputs("Empty input\n", stderr); return EXIT_FAILURE; }
    fclose(fp);
    /* strip trailing newline / CR */
    size_t id_len = strcspn(door_id, "\r\n");
    door_id[id_len] = '\0';

    char password[9] = {0};
    int found = 0;

    uint8_t digest[16];
    char buffer[256];

    for (unsigned long long index = 0; found < 8; ++index) {
        /* build string DoorID||index */
        int n = snprintf(buffer, sizeof buffer, "%s%llu", door_id, index);
        md5((uint8_t *)buffer, (size_t)n, digest);

        /* check first 20 bits == 0 => digest[0]==0 && digest[1]==0 && (digest[2] & 0xF0)==0 */
        if (digest[0] || digest[1] || (digest[2] & 0xF0)) continue;

        /* 6-th hex digit is low nibble of digest[2] */
        password[found++] = hex_digit(digest[2] & 0x0F);
    }

    printf("%s\n", password);
    return 0;
}
