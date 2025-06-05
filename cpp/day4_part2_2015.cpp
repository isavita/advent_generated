
#include <iostream>
#include <string>
#include <fstream>
#include <vector>
#include <iomanip>
#include <sstream>

typedef unsigned int uint32_t;
typedef unsigned long long uint64_t;
typedef unsigned char uint8_t;

#define F(x, y, z) (((x) & (y)) | ((~x) & (z)))
#define G(x, y, z) (((x) & (z)) | ((y) & (~z)))
#define H(x, y, z) ((x) ^ (y) ^ (z))
#define I(x, y, z) ((y) ^ ((x) | (~z)))

#define ROTATE_LEFT(x, n) (((x) << (n)) | ((x) >> (32 - (n))))

#define FF(a, b, c, d, x, s, ac) { \
    (a) += F((b), (c), (d)) + (x) + (uint32_t)(ac); \
    (a) = ROTATE_LEFT((a), (s)); \
    (a) += (b); \
}
#define GG(a, b, c, d, x, s, ac) { \
    (a) += G((b), (c), (d)) + (x) + (uint32_t)(ac); \
    (a) = ROTATE_LEFT((a), (s)); \
    (a) += (b); \
}
#define HH(a, b, c, d, x, s, ac) { \
    (a) += H((b), (c), (d)) + (x) + (uint32_t)(ac); \
    (a) = ROTATE_LEFT((a), (s)); \
    (a) += (b); \
}
#define II(a, b, c, d, x, s, ac) { \
    (a) += I((b), (c), (d)) + (x) + (uint32_t)(ac); \
    (a) = ROTATE_LEFT((a), (s)); \
    (a) += (b); \
}

static const uint32_t s[] = {
    7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,
    5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,
    4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,
    6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21
};

static const uint32_t k[] = {
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

static uint32_t decode_le(const uint8_t* buf, int offset) {
    return ((uint32_t)buf[offset]) |
           (((uint32_t)buf[offset + 1]) << 8) |
           (((uint32_t)buf[offset + 2]) << 16) |
           (((uint32_t)buf[offset + 3]) << 24);
}

static void encode_le(uint8_t* buf, uint32_t val, int offset) {
    buf[offset] = (uint8_t)(val & 0xFF);
    buf[offset + 1] = (uint8_t)((val >> 8) & 0xFF);
    buf[offset + 2] = (uint8_t)((val >> 16) & 0xFF);
    buf[offset + 3] = (uint8_t)((val >> 24) & 0xFF);
}

static void md5_transform(uint32_t state[4], const uint8_t block[64]) {
    uint32_t a = state[0], b = state[1], c = state[2], d = state[3];
    uint32_t x[16];

    for (int i = 0; i < 16; ++i) {
        x[i] = decode_le(block, i * 4);
    }

    FF (a, b, c, d, x[ 0], s[ 0], k[ 0]);
    FF (d, a, b, c, x[ 1], s[ 1], k[ 1]);
    FF (c, d, a, b, x[ 2], s[ 2], k[ 2]);
    FF (b, c, d, a, x[ 3], s[ 3], k[ 3]);
    FF (a, b, c, d, x[ 4], s[ 4], k[ 4]);
    FF (d, a, b, c, x[ 5], s[ 5], k[ 5]);
    FF (c, d, a, b, x[ 6], s[ 6], k[ 6]);
    FF (b, c, d, a, x[ 7], s[ 7], k[ 7]);
    FF (a, b, c, d, x[ 8], s[ 8], k[ 8]);
    FF (d, a, b, c, x[ 9], s[ 9], k[ 9]);
    FF (c, d, a, b, x[10], s[10], k[10]);
    FF (b, c, d, a, x[11], s[11], k[11]);
    FF (a, b, c, d, x[12], s[12], k[12]);
    FF (d, a, b, c, x[13], s[13], k[13]);
    FF (c, d, a, b, x[14], s[14], k[14]);
    FF (b, c, d, a, x[15], s[15], k[15]);

    GG (a, b, c, d, x[ 1], s[16], k[16]);
    GG (d, a, b, c, x[ 6], s[17], k[17]);
    GG (c, d, a, b, x[11], s[18], k[18]);
    GG (b, c, d, a, x[ 0], s[19], k[19]);
    GG (a, b, c, d, x[ 5], s[20], k[20]);
    GG (d, a, b, c, x[10], s[21], k[21]);
    GG (c, d, a, b, x[15], s[22], k[22]);
    GG (b, c, d, a, x[ 4], s[23], k[23]);
    GG (a, b, c, d, x[ 9], s[24], k[24]);
    GG (d, a, b, c, x[14], s[25], k[25]);
    GG (c, d, a, b, x[ 3], s[26], k[26]);
    GG (b, c, d, a, x[ 8], s[27], k[27]);
    GG (a, b, c, d, x[13], s[28], k[28]);
    GG (d, a, b, c, x[ 2], s[29], k[29]);
    GG (c, d, a, b, x[ 7], s[30], k[30]);
    GG (b, c, d, a, x[12], s[31], k[31]);

    HH (a, b, c, d, x[ 5], s[32], k[32]);
    HH (d, a, b, c, x[ 8], s[33], k[33]);
    HH (c, d, a, b, x[11], s[34], k[34]);
    HH (b, c, d, a, x[14], s[35], k[35]);
    HH (a, b, c, d, x[ 1], s[36], k[36]);
    HH (d, a, b, c, x[ 4], s[37], k[37]);
    HH (c, d, a, b, x[ 7], s[38], k[38]);
    HH (b, c, d, a, x[10], s[39], k[39]);
    HH (a, b, c, d, x[13], s[40], k[40]);
    HH (d, a, b, c, x[ 0], s[41], k[41]);
    HH (c, d, a, b, x[ 3], s[42], k[42]);
    HH (b, c, d, a, x[ 6], s[43], k[43]);
    HH (a, b, c, d, x[ 9], s[44], k[44]);
    HH (d, a, b, c, x[12], s[45], k[45]);
    HH (c, d, a, b, x[15], s[46], k[46]);
    HH (b, c, d, a, x[ 2], s[47], k[47]);

    II (a, b, c, d, x[ 0], s[48], k[48]);
    II (d, a, b, c, x[ 7], s[49], k[49]);
    II (c, d, a, b, x[14], s[50], k[50]);
    II (b, c, d, a, x[ 5], s[51], k[51]);
    II (a, b, c, d, x[12], s[52], k[52]);
    II (d, a, b, c, x[ 3], s[53], k[53]);
    II (c, d, a, b, x[10], s[54], k[54]);
    II (b, c, d, a, x[ 1], s[55], k[55]);
    II (a, b, c, d, x[ 8], s[56], k[56]);
    II (d, a, b, c, x[15], s[57], k[57]);
    II (c, d, a, b, x[ 6], s[58], k[58]);
    II (b, c, d, a, x[13], s[59], k[59]);
    II (a, b, c, d, x[ 4], s[60], k[60]);
    II (d, a, b, c, x[11], s[61], k[61]);
    II (c, d, a, b, x[ 2], s[62], k[62]);
    II (b, c, d, a, x[ 9], s[63], k[63]);

    state[0] += a;
    state[1] += b;
    state[2] += c;
    state[3] += d;
}

std::string md5(const std::string& input) {
    uint32_t state[4] = {
        0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476
    };
    uint64_t bit_len = input.length() * 8;
    std::vector<uint8_t> padded_input(input.begin(), input.end());

    padded_input.push_back(0x80);
    while (padded_input.size() % 64 != 56) {
        padded_input.push_back(0x00);
    }

    for (int i = 0; i < 8; ++i) {
        padded_input.push_back((uint8_t)(bit_len >> (i * 8)));
    }

    for (size_t i = 0; i < padded_input.size(); i += 64) {
        md5_transform(state, &padded_input[i]);
    }

    std::stringstream ss;
    uint8_t output_bytes[16];
    encode_le(output_bytes, state[0], 0);
    encode_le(output_bytes, state[1], 4);
    encode_le(output_bytes, state[2], 8);
    encode_le(output_bytes, state[3], 12);

    for(int i = 0; i < 16; ++i) {
        ss << std::hex << std::setw(2) << std::setfill('0') << (int)output_bytes[i];
    }
    return ss.str();
}

long long findAdventcoin(const std::string& secretKey, int numZeroes) {
    long long i = 1;
    std::string prefix(numZeroes, '0');
    while (true) {
        std::string key = secretKey + std::to_string(i);
        std::string hashKey = md5(key);
        if (hashKey.rfind(prefix, 0) == 0) {
            return i;
        }
        i++;
    }
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::ifstream file("input.txt");
    std::string secretKey;
    std::getline(file, secretKey);

    long long resultPart1 = findAdventcoin(secretKey, 5);
    long long resultPart2 = findAdventcoin(secretKey, 6);

    std::cout << resultPart1 << std::endl;
    std::cout << resultPart2 << std::endl;

    return 0;
}
