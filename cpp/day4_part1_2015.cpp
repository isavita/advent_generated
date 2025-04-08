
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <sstream>
#include <iomanip>
#include <cstring> // For memcpy

// --- Start of MD5 implementation (based on common reference implementations) ---
// Copyright (C) 1991-2, RSA Data Security, Inc. Created 1991. All rights reserved.
// License to copy and use this software is granted provided that it
// is identified as the "RSA Data Security, Inc. MD5 Message-Digest
// Algorithm" in all material mentioning or referencing this software
// or this function.
// License is also granted to make and use derivative works provided
// that such works are identified as "derived from the RSA Data
// Security, Inc. MD5 Message-Digest Algorithm" in all material
// mentioning or referencing the derived work.
// RSA Data Security, Inc. makes no representations concerning either
// the merchantability of this software or the suitability of this
// software for any particular purpose. It is provided "as is"
// without express or implied warranty of any kind.
// These notices must be retained in any copies of any part of this
// documentation and/or software.

typedef unsigned int uint32;
typedef unsigned char uint8;

class MD5 {
    uint32 state[4];
    uint32 count[2];
    uint8 buffer[64];
    uint8 PADDING[64];

    void init() {
        count[0] = count[1] = 0;
        state[0] = 0x67452301;
        state[1] = 0xefcdab89;
        state[2] = 0x98badcfe;
        state[3] = 0x10325476;
        memset(PADDING, 0, sizeof(PADDING));
        PADDING[0] = 0x80;
    }

    static inline uint32 F(uint32 x, uint32 y, uint32 z) { return (x & y) | (~x & z); }
    static inline uint32 G(uint32 x, uint32 y, uint32 z) { return (x & z) | (y & ~z); }
    static inline uint32 H(uint32 x, uint32 y, uint32 z) { return x ^ y ^ z; }
    static inline uint32 I(uint32 x, uint32 y, uint32 z) { return y ^ (x | ~z); }

    static inline uint32 rotate_left(uint32 x, uint32 n) { return (x << n) | (x >> (32 - n)); }

    static inline void FF(uint32 &a, uint32 b, uint32 c, uint32 d, uint32 x, uint32 s, uint32 ac) {
        a += F(b, c, d) + x + ac;
        a = rotate_left(a, s);
        a += b;
    }
    static inline void GG(uint32 &a, uint32 b, uint32 c, uint32 d, uint32 x, uint32 s, uint32 ac) {
        a += G(b, c, d) + x + ac;
        a = rotate_left(a, s);
        a += b;
    }
    static inline void HH(uint32 &a, uint32 b, uint32 c, uint32 d, uint32 x, uint32 s, uint32 ac) {
        a += H(b, c, d) + x + ac;
        a = rotate_left(a, s);
        a += b;
    }
    static inline void II(uint32 &a, uint32 b, uint32 c, uint32 d, uint32 x, uint32 s, uint32 ac) {
        a += I(b, c, d) + x + ac;
        a = rotate_left(a, s);
        a += b;
    }

    void transform(const uint8 block[64]) {
        uint32 a = state[0], b = state[1], c = state[2], d = state[3], x[16];
        decode(x, block, 64);

        FF(a, b, c, d, x[ 0], 7, 0xd76aa478); FF(d, a, b, c, x[ 1], 12, 0xe8c7b756); FF(c, d, a, b, x[ 2], 17, 0x242070db); FF(b, c, d, a, x[ 3], 22, 0xc1bdceee);
        FF(a, b, c, d, x[ 4], 7, 0xf57c0faf); FF(d, a, b, c, x[ 5], 12, 0x4787c62a); FF(c, d, a, b, x[ 6], 17, 0xa8304613); FF(b, c, d, a, x[ 7], 22, 0xfd469501);
        FF(a, b, c, d, x[ 8], 7, 0x698098d8); FF(d, a, b, c, x[ 9], 12, 0x8b44f7af); FF(c, d, a, b, x[10], 17, 0xffff5bb1); FF(b, c, d, a, x[11], 22, 0x895cd7be);
        FF(a, b, c, d, x[12], 7, 0x6b901122); FF(d, a, b, c, x[13], 12, 0xfd987193); FF(c, d, a, b, x[14], 17, 0xa679438e); FF(b, c, d, a, x[15], 22, 0x49b40821);

        GG(a, b, c, d, x[ 1], 5, 0xf61e2562); GG(d, a, b, c, x[ 6], 9, 0xc040b340); GG(c, d, a, b, x[11], 14, 0x265e5a51); GG(b, c, d, a, x[ 0], 20, 0xe9b6c7aa);
        GG(a, b, c, d, x[ 5], 5, 0xd62f105d); GG(d, a, b, c, x[10], 9,  0x2441453); GG(c, d, a, b, x[15], 14, 0xd8a1e681); GG(b, c, d, a, x[ 4], 20, 0xe7d3fbc8);
        GG(a, b, c, d, x[ 9], 5, 0x21e1cde6); GG(d, a, b, c, x[14], 9, 0xc33707d6); GG(c, d, a, b, x[ 3], 14, 0xf4d50d87); GG(b, c, d, a, x[ 8], 20, 0x455a14ed);
        GG(a, b, c, d, x[13], 5, 0xa9e3e905); GG(d, a, b, c, x[ 2], 9, 0xfcefa3f8); GG(c, d, a, b, x[ 7], 14, 0x676f02d9); GG(b, c, d, a, x[12], 20, 0x8d2a4c8a);

        HH(a, b, c, d, x[ 5], 4, 0xfffa3942); HH(d, a, b, c, x[ 8], 11, 0x8771f681); HH(c, d, a, b, x[11], 16, 0x6d9d6122); HH(b, c, d, a, x[14], 23, 0xfde5380c);
        HH(a, b, c, d, x[ 1], 4, 0xa4beea44); HH(d, a, b, c, x[ 4], 11, 0x4bdecfa9); HH(c, d, a, b, x[ 7], 16, 0xf6bb4b60); HH(b, c, d, a, x[10], 23, 0xbebfbc70);
        HH(a, b, c, d, x[13], 4, 0x289b7ec6); HH(d, a, b, c, x[ 0], 11, 0xeaa127fa); HH(c, d, a, b, x[ 3], 16, 0xd4ef3085); HH(b, c, d, a, x[ 6], 23,  0x4881d05);
        HH(a, b, c, d, x[ 9], 4, 0xd9d4d039); HH(d, a, b, c, x[12], 11, 0xe6db99e5); HH(c, d, a, b, x[15], 16, 0x1fa27cf8); HH(b, c, d, a, x[ 2], 23, 0xc4ac5665);

        II(a, b, c, d, x[ 0], 6, 0xf4292244); II(d, a, b, c, x[ 7], 10, 0x432aff97); II(c, d, a, b, x[14], 15, 0xab9423a7); II(b, c, d, a, x[ 5], 21, 0xfc93a039);
        II(a, b, c, d, x[12], 6, 0x655b59c3); II(d, a, b, c, x[ 3], 10, 0x8f0ccc92); II(c, d, a, b, x[10], 15, 0xffeff47d); II(b, c, d, a, x[ 1], 21, 0x85845dd1);
        II(a, b, c, d, x[ 8], 6, 0x6fa87e4f); II(d, a, b, c, x[15], 10, 0xfe2ce6e0); II(c, d, a, b, x[ 6], 15, 0xa3014314); II(b, c, d, a, x[13], 21, 0x4e0811a1);
        II(a, b, c, d, x[ 4], 6, 0xf7537e82); II(d, a, b, c, x[11], 10, 0xbd3af235); II(c, d, a, b, x[ 2], 15, 0x2ad7d2bb); II(b, c, d, a, x[ 9], 21, 0xeb86d391);

        state[0] += a; state[1] += b; state[2] += c; state[3] += d;
    }

    void encode(uint8 *output, const uint32 *input, size_t len) {
        for (size_t i = 0, j = 0; j < len; i++, j += 4) {
            output[j] = (uint8)(input[i] & 0xff);
            output[j+1] = (uint8)((input[i] >> 8) & 0xff);
            output[j+2] = (uint8)((input[i] >> 16) & 0xff);
            output[j+3] = (uint8)((input[i] >> 24) & 0xff);
        }
    }

    void decode(uint32 *output, const uint8 *input, size_t len) {
        for (size_t i = 0, j = 0; j < len; i++, j += 4) {
            output[i] = ((uint32)input[j]) | (((uint32)input[j+1]) << 8) |
                       (((uint32)input[j+2]) << 16) | (((uint32)input[j+3]) << 24);
        }
    }

public:
    MD5() {
        init();
    }

    void update(const uint8 *input, size_t inputLen) {
        size_t index = (unsigned int)((count[0] >> 3) & 0x3F);
        if ((count[0] += ((uint32)inputLen << 3)) < ((uint32)inputLen << 3))
            count[1]++;
        count[1] += ((uint32)inputLen >> 29);

        size_t partLen = 64 - index;
        size_t i = 0;

        if (inputLen >= partLen) {
            memcpy(&buffer[index], input, partLen);
            transform(buffer);
            for (i = partLen; i + 63 < inputLen; i += 64)
                transform(&input[i]);
            index = 0;
        }
        memcpy(&buffer[index], &input[i], inputLen - i);
    }

    void update(const char *input, size_t inputLen) {
        update((const uint8 *)input, inputLen);
    }
    
    void update(const std::string& str) {
         update((const uint8*)str.c_str(), str.length());
    }

    void finalize(uint8 digest[16]) {
        uint8 bits[8];
        encode(bits, count, 8);
        size_t index = (unsigned int)((count[0] >> 3) & 0x3f);
        size_t padLen = (index < 56) ? (56 - index) : (120 - index);
        update(PADDING, padLen);
        update(bits, 8);
        encode(digest, state, 16);
        init(); // Reset state for potential reuse
    }

    std::string final_hex() {
        uint8 digest[16];
        finalize(digest);
        std::stringstream ss;
        ss << std::hex << std::setfill('0');
        for(int i=0; i<16; ++i) {
            ss << std::setw(2) << (unsigned int)digest[i];
        }
        return ss.str();
    }
    
    // Function to compute MD5 directly for a string
    static std::string compute(const std::string& str) {
        MD5 md5;
        md5.update(str);
        return md5.final_hex();
    }

     // Function to compute MD5 and return raw bytes
    static void compute_raw(const std::string& str, uint8 digest[16]) {
        MD5 md5;
        md5.update(str);
        md5.finalize(digest);
    }
};
// --- End of MD5 implementation ---


int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::ifstream inputFile("input.txt");
    if (!inputFile) {
         // Error handling if needed, but prompt requested no explanation/comments
         return 1;
    }

    std::string secret_key;
    std::getline(inputFile, secret_key);
    inputFile.close();

    long long number = 0;
    uint8 digest[16];

    while (true) {
        number++;
        std::string input_string = secret_key + std::to_string(number);
        MD5::compute_raw(input_string, digest);

        // Check first 2.5 bytes (5 hex characters) for zero
        // 00000... -> digest[0] == 0, digest[1] == 0, digest[2]'s high nibble == 0
        if (digest[0] == 0 && digest[1] == 0 && (digest[2] & 0xF0) == 0) {
            std::cout << number << "\n";
            break;
        }
    }

    return 0;
}
