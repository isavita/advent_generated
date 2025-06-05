
#include <iostream>
#include <string>
#include <vector>
#include <fstream>
#include <iomanip>
#include <sstream>
#include <cctype> // For std::isdigit

// MD5 Implementation (Self-contained)
typedef unsigned char byte;
typedef unsigned int uint32;

struct MD5_CTX {
    uint32 state[4];
    uint32 count[2];
    byte buffer[64];
};

namespace {
    void MD5Transform(uint32 state[4], const byte block[64]);
    void Encode(byte* output, const uint32* input, uint32 len);
    void Decode(uint32* output, const byte* input, uint32 len);
    void MD5_memcpy(byte* dest, const byte* src, uint32 len);
    void MD5_memset(byte* dest, int val, uint32 len);

    inline uint32 F(uint32 x, uint32 y, uint32 z) { return (x & y) | (~x & z); }
    inline uint32 G(uint32 x, uint32 y, uint32 z) { return (x & z) | (y & ~z); }
    inline uint32 H(uint32 x, uint32 y, uint32 z) { return x ^ y ^ z; }
    inline uint32 I(uint32 x, uint32 y, uint32 z) { return y ^ (x | ~z); }

    inline uint32 ROTATE_LEFT(uint32 x, int n) { return (x << n) | (x >> (32 - n)); }

    void FF(uint32& a, uint32 b, uint32 c, uint32 d, uint32 x, int s, uint32 ac) {
        a += F(b, c, d) + x + ac;
        a = ROTATE_LEFT(a, s);
        a += b;
    }
    void GG(uint32& a, uint32 b, uint32 c, uint32 d, uint32 x, int s, uint32 ac) {
        a += G(b, c, d) + x + ac;
        a = ROTATE_LEFT(a, s);
        a += b;
    }
    void HH(uint32& a, uint32 b, uint32 c, uint32 d, uint32 x, int s, uint32 ac) {
        a += H(b, c, d) + x + ac;
        a = ROTATE_LEFT(a, s);
        a += b;
    }
    void II(uint32& a, uint32 b, uint32 c, uint32 d, uint32 x, int s, uint32 ac) {
        a += I(b, c, d) + x + ac;
        a = ROTATE_LEFT(a, s);
        a += b;
    }

    static const uint32 S11 = 7, S12 = 12, S13 = 17, S14 = 22;
    static const uint32 S21 = 5, S22 = 9, S23 = 14, S24 = 20;
    static const uint32 S31 = 4, S32 = 11, S33 = 16, S34 = 23;
    static const uint32 S41 = 6, S42 = 10, S43 = 15, S44 = 21;

    void MD5_memcpy(byte* dest, const byte* src, uint32 len) {
        for (uint32 i = 0; i < len; ++i) dest[i] = src[i];
    }

    void MD5_memset(byte* dest, int val, uint32 len) {
        for (uint32 i = 0; i < len; ++i) dest[i] = (byte)val;
    }

    void Encode(byte* output, const uint32* input, uint32 len) {
        for (uint32 i = 0, j = 0; j < len; ++i, j += 4) {
            output[j] = (byte)(input[i] & 0xff);
            output[j + 1] = (byte)((input[i] >> 8) & 0xff);
            output[j + 2] = (byte)((input[i] >> 16) & 0xff);
            output[j + 3] = (byte)((input[i] >> 24) & 0xff);
        }
    }

    void Decode(uint32* output, const byte* input, uint32 len) {
        for (uint32 i = 0, j = 0; j < len; ++i, j += 4) {
            output[i] = ((uint32)input[j]) | (((uint32)input[j + 1]) << 8) |
                        (((uint32)input[j + 2]) << 16) | (((uint32)input[j + 3]) << 24);
        }
    }

    void MD5Transform(uint32 state[4], const byte block[64]) {
        uint32 a = state[0], b = state[1], c = state[2], d = state[3];
        uint32 x[16];

        Decode(x, block, 64);

        FF(a, b, c, d, x[0], S11, 0xd76aa478); FF(d, a, b, c, x[1], S12, 0xe8c7b756);
        FF(c, d, a, b, x[2], S13, 0x242070db); FF(b, c, d, a, x[3], S14, 0xc1bdceee);
        FF(a, b, c, d, x[4], S11, 0xf57c0faf); FF(d, a, b, c, x[5], S12, 0x4787c62a);
        FF(c, d, a, b, x[6], S13, 0xa8304613); FF(b, c, d, a, x[7], S14, 0xfd469501);
        FF(a, b, c, d, x[8], S11, 0x698098d8); FF(d, a, b, c, x[9], S12, 0x8b44f7af);
        FF(c, d, a, b, x[10], S13, 0xffff5bb1); FF(b, c, d, a, x[11], S14, 0x895cd7be);
        FF(a, b, c, d, x[12], S11, 0x6b901122); FF(d, a, b, c, x[13], S12, 0xfd987193);
        FF(c, d, a, b, x[14], S13, 0xa679438e); FF(b, c, d, a, x[15], S14, 0x49b40821);

        GG(a, b, c, d, x[1], S21, 0xf61e2562); GG(d, a, b, c, x[6], S22, 0xc040b340);
        GG(c, d, a, b, x[11], S23, 0x265e5a51); GG(b, c, d, a, x[0], S24, 0xe9b6c7aa);
        GG(a, b, c, d, x[5], S21, 0xd62f105d); GG(d, a, b, c, x[10], S22, 0x2441453);
        GG(c, d, a, b, x[15], S23, 0xd8a1e681); GG(b, c, d, a, x[4], S24, 0xe7d3fbc8);
        GG(a, b, c, d, x[9], S21, 0x21e1cde6); GG(d, a, b, c, x[14], S22, 0xc33707d6);
        GG(c, d, a, b, x[3], S23, 0xf4d50d87); GG(b, c, d, a, x[8], S24, 0x455a14ed);
        GG(a, b, c, d, x[13], S21, 0xa9e3e905); GG(d, a, b, c, x[2], S22, 0xfcefa3f8);
        GG(c, d, a, b, x[7], S23, 0x676f02d9); GG(b, c, d, a, x[12], S24, 0x8d2a4c8a);

        HH(a, b, c, d, x[5], S31, 0xfffa3942); HH(d, a, b, c, x[8], S32, 0x8771f681);
        HH(c, d, a, b, x[11], S33, 0x6d9d6122); HH(b, c, d, a, x[14], S34, 0xfde5380c);
        HH(a, b, c, d, x[1], S31, 0xa4beea44); HH(d, a, b, c, x[4], S32, 0x4bdecfa9);
        HH(c, d, a, b, x[7], S33, 0xf6bb4b60); HH(b, c, d, a, x[10], S34, 0xbebfbc70);
        HH(a, b, c, d, x[13], S31, 0x289b7ec6); HH(d, a, b, c, x[0], S32, 0xeaa127fa);
        HH(c, d, a, b, x[3], S33, 0xd4ef3085); HH(b, c, d, a, x[6], S34, 0x4881d05);
        HH(a, b, c, d, x[9], S31, 0xd9d4d039); HH(d, a, b, c, x[12], S32, 0xe6db99e5);
        HH(c, d, a, b, x[15], S33, 0x1fa27cf8); HH(b, c, d, a, x[2], S34, 0xc4ac5665);

        II(a, b, c, d, x[0], S41, 0xf4292244); II(d, a, b, c, x[7], S42, 0x432aff97);
        II(c, d, a, b, x[14], S43, 0xab9423a7); II(b, c, d, a, x[5], S44, 0xfc93a039);
        II(a, b, c, d, x[12], S41, 0x655b59c3); II(d, a, b, c, x[3], S42, 0x8f0ccc92);
        II(c, d, a, b, x[10], S43, 0xffeff47d); II(b, c, d, a, x[1], S44, 0x85845dd1);
        II(a, b, c, d, x[8], S41, 0x6fa87e4f); II(d, a, b, c, x[15], S42, 0xfe2ce6e0);
        II(c, d, a, b, x[6], S43, 0xa3014314); II(b, c, d, a, x[13], S44, 0x4e0811a1);
        II(a, b, c, d, x[4], S41, 0xf7537e82); II(d, a, b, c, x[11], S42, 0xbd3af235);
        II(c, d, a, b, x[2], S43, 0x2ad7d2bb); II(b, c, d, a, x[9], S44, 0xeb86d391);

        state[0] += a; state[1] += b; state[2] += c; state[3] += d;
        MD5_memset((byte*)x, 0, sizeof(x));
    }
}

void MD5_Init(MD5_CTX* context) {
    context->count[0] = context->count[1] = 0;
    context->state[0] = 0x67452301;
    context->state[1] = 0xefcdab89;
    context->state[2] = 0x98badcfe;
    context->state[3] = 0x10325476;
}

void MD5_Update(MD5_CTX* context, const byte* input, uint32 inputLen) {
    uint32 i, index, partLen;

    index = (uint32)((context->count[0] >> 3) & 0x3F);
    if ((context->count[0] += ((uint32)inputLen << 3)) < ((uint32)inputLen << 3))
        context->count[1]++;
    context->count[1] += ((uint32)inputLen >> 29);

    partLen = 64 - index;

    if (inputLen >= partLen) {
        MD5_memcpy(&context->buffer[index], input, partLen);
        MD5Transform(context->state, context->buffer);

        for (i = partLen; i + 63 < inputLen; i += 64)
            MD5Transform(context->state, &input[i]);
        index = 0;
    } else {
        i = 0;
    }
    MD5_memcpy(&context->buffer[index], &input[i], inputLen - i);
}

void MD5_Final(byte digest[16], MD5_CTX* context) {
    byte final_block[64];
    uint32 last_block_len = (context->count[0] >> 3) % 64;

    MD5_memset(final_block, 0, 64);
    MD5_memcpy(final_block, context->buffer, last_block_len);

    final_block[last_block_len] = 0x80;

    if (last_block_len >= 56) {
        MD5Transform(context->state, final_block);
        MD5_memset(final_block, 0, 64);
    }
    Encode(final_block + 56, context->count, 8);
    MD5Transform(context->state, final_block);
    Encode(digest, context->state, 16);
    MD5_memset((byte*)context, 0, sizeof(*context));
}

std::string md5_hash(const std::string& input) {
    MD5_CTX context;
    byte digest[16];

    MD5_Init(&context);
    MD5_Update(&context, (const byte*)input.c_str(), input.length());
    MD5_Final(digest, &context);

    std::stringstream ss;
    for (int i = 0; i < 16; ++i) {
        ss << std::hex << std::setw(2) << std::setfill('0') << (int)digest[i];
    }
    return ss.str();
}

std::string find_password(const std::string& door_id) {
    std::string password(8, ' ');
    std::vector<bool> found(8, false);
    int filled_positions = 0;
    long long i = 0;

    while (filled_positions < 8) {
        std::string current_input = door_id + std::to_string(i);
        std::string hash_str = md5_hash(current_input);

        if (hash_str.rfind("00000", 0) == 0) {
            if (hash_str.length() >= 7) {
                char pos_char = hash_str[5];
                if (std::isdigit(pos_char)) {
                    int pos_index = pos_char - '0';
                    if (pos_index >= 0 && pos_index < 8) {
                        if (!found[pos_index]) {
                            found[pos_index] = true;
                            password[pos_index] = hash_str[6];
                            filled_positions++;
                        }
                    }
                }
            }
        }
        i++;
    }
    return password;
}

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) return 1;

    std::string door_id;
    std::getline(file, door_id);
    file.close();

    size_t first = door_id.find_first_not_of(" \t\n\r\f\v");
    if (std::string::npos == first) {
        door_id = "";
    } else {
        size_t last = door_id.find_last_not_of(" \t\n\r\f\v");
        door_id = door_id.substr(first, (last - first + 1));
    }

    std::string password = find_password(door_id);
    std::cout << password << std::endl;

    return 0;
}
