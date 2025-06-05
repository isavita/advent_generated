
#include <iostream>
#include <string>
#include <fstream>
#include <iomanip>
#include <sstream>
#include <cstring> // For memcpy, memset

typedef unsigned int uint32;
typedef unsigned char uint8;

struct MD5_CTX {
    uint32 state[4];
    uint32 count[2];
    uint8 buffer[64];
};

#define F(x, y, z) (((x) & (y)) | ((~x) & (z)))
#define G(x, y, z) (((x) & (z)) | ((y) & (~z)))
#define H(x, y, z) ((x) ^ (y) ^ (z))
#define I(x, y, z) ((y) ^ ((x) | (~z)))

#define ROTATE_LEFT(x, n) (((x) << (n)) | ((x) >> (32 - (n))))

#define FF(a, b, c, d, x, s, ac) { \
    (a) += F ((b), (c), (d)) + (x) + (uint32)(ac); \
    (a) = ROTATE_LEFT ((a), (s)); \
    (a) += (b); \
}
#define GG(a, b, c, d, x, s, ac) { \
    (a) += G ((b), (c), (d)) + (x) + (uint32)(ac); \
    (a) = ROTATE_LEFT ((a), (s)); \
    (a) += (b); \
}
#define HH(a, b, c, d, x, s, ac) { \
    (a) += H ((b), (c), (d)) + (x) + (uint32)(ac); \
    (a) = ROTATE_LEFT ((a), (s)); \
    (a) += (b); \
}
#define II(a, b, c, d, x, s, ac) { \
    (a) += I ((b), (c), (d)) + (x) + (uint32)(ac); \
    (a) = ROTATE_LEFT ((a), (s)); \
    (a) += (b); \
}

static uint8 PADDING[64] = {
    0x80, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
};

static void MD5Init(MD5_CTX *context) {
    context->count[0] = context->count[1] = 0;
    context->state[0] = 0x67452301;
    context->state[1] = 0xEFCDAB89;
    context->state[2] = 0x98BADCFE;
    context->state[3] = 0x10325476;
}

static void Encode(uint8 *output, uint32 *input, uint32 len) {
    for (uint32 i = 0, j = 0; j < len; i++, j += 4) {
        output[j] = (uint8)(input[i] & 0xff);
        output[j+1] = (uint8)((input[i] >> 8) & 0xff);
        output[j+2] = (uint8)((input[i] >> 16) & 0xff);
        output[j+3] = (uint8)((input[i] >> 24) & 0xff);
    }
}

static void Decode(uint32 *output, uint8 *input, uint32 len) {
    for (uint32 i = 0, j = 0; j < len; i++, j += 4) {
        output[i] = ((uint32)input[j]) | (((uint32)input[j+1]) << 8) |
                    (((uint32)input[j+2]) << 16) | (((uint32)input[j+3]) << 24);
    }
}

static void MD5Transform(uint32 state[4], uint8 block[64]) {
    uint32 a = state[0], b = state[1], c = state[2], d = state[3];
    uint32 x[16];

    Decode(x, block, 64);

    FF (a, b, c, d, x[ 0], 7, 0xd76aa478); FF (d, a, b, c, x[ 1], 12, 0xe8c7b756);
    FF (c, d, a, b, x[ 2], 17, 0x242070db); FF (b, c, d, a, x[ 3], 22, 0xc1bdceee);
    FF (a, b, c, d, x[ 4], 7, 0xf57c0faf); FF (d, a, b, c, x[ 5], 12, 0x4787c62a);
    FF (c, d, a, b, x[ 6], 17, 0xa8304613); FF (b, c, d, a, x[ 7], 22, 0xfd469501);
    FF (a, b, c, d, x[ 8], 7, 0x698098d8); FF (d, a, b, c, x[ 9], 12, 0x8b44f7af);
    FF (c, d, a, b, x[10], 17, 0xffff5bb1); FF (b, c, d, a, x[11], 22, 0x895cd7be);
    FF (a, b, c, d, x[12], 7, 0x6b901122); FF (d, a, b, c, x[13], 12, 0xfd987193);
    FF (c, d, a, b, x[14], 17, 0xa679438e); FF (b, c, d, a, x[15], 22, 0x49b40821);

    GG (a, b, c, d, x[ 1], 5, 0xf61e2562); GG (d, a, b, c, x[ 6], 9, 0xc040b340);
    GG (c, d, a, b, x[11], 14, 0x265e5a51); GG (b, c, d, a, x[ 0], 20, 0xe9b6c7aa);
    GG (a, b, c, d, x[ 5], 5, 0xd62f105d); GG (d, a, b, c, x[10], 9,  0x2441453);
    GG (c, d, a, b, x[15], 14, 0xd8a1e681); GG (b, c, d, a, x[ 4], 20, 0xe7d3fbc8);
    GG (a, b, c, d, x[ 9], 5, 0x21e1cde6); GG (d, a, b, c, x[14], 9, 0xc33707d6);
    GG (c, d, a, b, x[ 3], 14, 0xf4d50d87); GG (b, c, d, a, x[ 8], 20, 0x455a14ed);
    GG (a, b, c, d, x[13], 5, 0xa9e3e905); GG (d, a, b, c, x[ 2], 9, 0xfcefa3f8);
    GG (c, d, a, b, x[ 7], 14, 0x676f02d9); GG (b, c, d, a, x[12], 20, 0x8d2a4c8a);

    HH (a, b, c, d, x[ 5], 4, 0xfffa3942); HH (d, a, b, c, x[ 8], 11, 0x8771f681);
    HH (c, d, a, b, x[11], 16, 0x6d9d6122); HH (b, c, d, a, x[14], 23, 0xfde5380c);
    HH (a, b, c, d, x[ 1], 4, 0xa4beea44); HH (d, a, b, c, x[ 4], 11, 0x4bdecfa9);
    HH (c, d, a, b, x[ 7], 16, 0xf6bb4b60); HH (b, c, d, a, x[10], 23, 0xbebfbc70);
    HH (a, b, c, d, x[13], 4, 0x289b7ec6); HH (d, a, b, c, x[ 0], 11, 0xeaa127fa);
    HH (c, d, a, b, x[ 3], 16, 0xd4ef3085); HH (b, c, d, a, x[ 6], 23, 0x4881d05);
    HH (a, b, c, d, x[ 9], 4, 0xd9d4d039); HH (d, a, b, c, x[12], 11, 0xe6db99e5);
    HH (c, d, a, b, x[15], 16, 0x1fa27cf8); HH (b, c, d, a, x[ 2], 23, 0xc4ac5665);

    II (a, b, c, d, x[ 0], 6, 0xf4292244); II (d, a, b, c, x[ 7], 10, 0x432aff97);
    II (c, d, a, b, x[14], 15, 0xab9423a7); II (b, c, d, a, x[ 5], 21, 0xfc93a039);
    II (a, b, c, d, x[12], 6, 0x655b59c3); II (d, a, b, c, x[ 3], 10, 0x8f0ccc92);
    II (c, d, a, b, x[10], 15, 0xffeff47d); II (b, c, d, a, x[ 1], 21, 0x85845dd1);
    II (a, b, c, d, x[ 8], 6, 0x6fa87e4f); II (d, a, b, c, x[15], 10, 0xfe2ce6e0);
    II (c, d, a, b, x[ 6], 15, 0xa3014314); II (b, c, d, a, x[13], 21, 0x4e0811a1);
    II (a, b, c, d, x[ 4], 6, 0xf7537e82); II (d, a, b, c, x[11], 10, 0xbd3af235);
    II (c, d, a, b, x[ 2], 15, 0x2ad7d2bb); II (b, c, d, a, x[ 9], 21, 0xeb86d391);

    state[0] += a;
    state[1] += b;
    state[2] += c;
    state[3] += d;
}

static void MD5Update(MD5_CTX *context, const uint8 *input, uint32 inputLen) {
    uint32 i, index, partLen;

    index = (uint32)((context->count[0] >> 3) & 0x3F);

    if ((context->count[0] += ((uint32)inputLen << 3)) < ((uint32)inputLen << 3))
        context->count[1]++;
    context->count[1] += ((uint32)inputLen >> 29);

    partLen = 64 - index;

    if (inputLen >= partLen) {
        memcpy((void *)&context->buffer[index], (void *)input, partLen);
        MD5Transform(context->state, context->buffer);

        for (i = partLen; i + 63 < inputLen; i += 64)
            MD5Transform(context->state, (uint8*)&input[i]);

        index = 0;
    } else {
        i = 0;
    }

    memcpy((void *)&context->buffer[index], (void *)&input[i], inputLen-i);
}

static void MD5Final(uint8 digest[16], MD5_CTX *context) {
    uint8 bits[8];
    uint32 index, padLen;

    Encode(bits, context->count, 8);

    index = (uint32)((context->count[0] >> 3) & 0x3f);
    padLen = (index < 56) ? (56 - index) : (120 - index);
    MD5Update(context, PADDING, padLen);

    MD5Update(context, bits, 8);

    Encode(digest, context->state, 16);

    memset((void *)context, 0, sizeof (*context));
}

std::string md5_hexdigest(const std::string& input) {
    MD5_CTX context;
    uint8 digest[16];

    MD5Init(&context);
    MD5Update(&context, (const uint8*)input.c_str(), input.length());
    MD5Final(digest, &context);

    std::ostringstream ss;
    for (int i = 0; i < 16; ++i) {
        ss << std::hex << std::setw(2) << std::setfill('0') << (int)digest[i];
    }
    return ss.str();
}

int main() {
    std::ifstream file("input.txt");
    std::string door_id;
    if (!file.is_open()) return 1;
    std::getline(file, door_id);
    file.close();

    std::string password = "";
    long long index = 0;

    while (password.length() < 8) {
        std::string hash_input = door_id + std::to_string(index);
        std::string result = md5_hexdigest(hash_input);
        
        if (result[0] == '0' &&
            result[1] == '0' &&
            result[2] == '0' &&
            result[3] == '0' &&
            result[4] == '0') {
            password += result[5];
        }
        
        index++;
    }

    std::cout << password << std::endl;

    return 0;
}
