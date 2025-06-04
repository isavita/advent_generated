
#include <iostream>
#include <string>
#include <vector>
#include <fstream>
#include <sstream>
#include <iomanip>

typedef unsigned int MD5_u32;

#define F(x, y, z) (((x) & (y)) | ((~x) & (z)))
#define G(x, y, z) (((x) & (z)) | ((y) & (~z)))
#define H(x, y, z) ((x) ^ (y) ^ (z))
#define I(x, y, z) ((y) ^ ((x) | (~z)))

#define ROTATE_LEFT(x, n) (((x) << (n)) | ((x) >> (32 - (n))))

#define FF(a, b, c, d, x, s, ac) { \
    (a) += F((b), (c), (d)) + (x) + (MD5_u32)(ac); \
    (a) = ROTATE_LEFT((a), (s)); \
    (a) += (b); \
}
#define GG(a, b, c, d, x, s, ac) { \
    (a) += G((b), (c), (d)) + (x) + (MD5_u32)(ac); \
    (a) = ROTATE_LEFT((a), (s)); \
    (a) += (b); \
}
#define HH(a, b, c, d, x, s, ac) { \
    (a) += H((b), (c), (d)) + (x) + (MD5_u32)(ac); \
    (a) = ROTATE_LEFT((a), (s)); \
    (a) += (b); \
}
#define II(a, b, c, d, x, s, ac) { \
    (a) += I((b), (c), (d)) + (x) + (MD5_u32)(ac); \
    (a) = ROTATE_LEFT((a), (s)); \
    (a) += (b); \
}

static void md5_transform(MD5_u32 state[4], const unsigned char block[64]) {
    MD5_u32 a = state[0], b = state[1], c = state[2], d = state[3];
    MD5_u32 x[16];

    for (int i = 0; i < 16; ++i) {
        x[i] = ((MD5_u32)block[i * 4]) |
               ((MD5_u32)block[i * 4 + 1] << 8) |
               ((MD5_u32)block[i * 4 + 2] << 16) |
               ((MD5_u32)block[i * 4 + 3] << 24);
    }

    FF(a, b, c, d, x[ 0],  7, 0xd76aa478); FF(d, a, b, c, x[ 1], 12, 0xe8c7b756);
    FF(c, d, a, b, x[ 2], 17, 0x242070db); FF(b, c, d, a, x[ 3], 22, 0xc1bdceee);
    FF(a, b, c, d, x[ 4],  7, 0xf57c0faf); FF(d, a, b, c, x[ 5], 12, 0x4787c62a);
    FF(c, d, a, b, x[ 6], 17, 0xa8304613); FF(b, c, d, a, x[ 7], 22, 0xfd469501);
    FF(a, b, c, d, x[ 8],  7, 0x698098d8); FF(d, a, b, c, x[ 9], 12, 0x8b44f7af);
    FF(c, d, a, b, x[10], 17, 0xffff5bb1); FF(b, c, d, a, x[11], 22, 0x895cd7be);
    FF(a, b, c, d, x[12],  7, 0x6b901122); FF(d, a, b, c, x[13], 12, 0xfd987193);
    FF(c, d, a, b, x[14], 17, 0xa679438e); FF(b, c, d, a, x[15], 22, 0x49b40821);

    GG(a, b, c, d, x[ 1],  5, 0xf61e2562); GG(d, a, b, c, x[ 6],  9, 0xc040b340);
    GG(c, d, a, b, x[11], 14, 0x265e5a51); GG(b, c, d, a, x[ 0], 20, 0xe9b6c7aa);
    GG(a, b, c, d, x[ 5],  5, 0xd62f105d); GG(d, a, b, c, x[10],  9, 0x02441453);
    GG(c, d, a, b, x[15], 14, 0xd8a1e681); GG(b, c, d, a, x[ 4], 20, 0xe7d3fbc8);
    GG(a, b, c, d, x[ 9],  5, 0x21e1cde6); GG(d, a, b, c, x[14],  9, 0xc33707d6);
    GG(c, d, a, b, x[ 3], 14, 0xf4d50d87); GG(b, c, d, a, x[ 8], 20, 0x455a14ed);
    GG(a, b, c, d, x[13],  5, 0xa9e3e905); GG(d, a, b, c, x[ 2],  9, 0xfcefa3f8);
    GG(c, d, a, b, x[ 7], 14, 0x676f02d9); GG(b, c, d, a, x[12], 20, 0x8d2a4c8a);

    HH(a, b, c, d, x[ 5],  4, 0xfffa3942); HH(d, a, b, c, x[ 8], 11, 0x8771f681);
    HH(c, d, a, b, x[11], 16, 0x6d9d6122); HH(b, c, d, a, x[14], 23, 0xfde5380c);
    HH(a, b, c, d, x[ 1],  4, 0xa4beea44); HH(d, a, b, c, x[ 4], 11, 0x4bdecfa9);
    HH(c, d, a, b, x[ 7], 16, 0xf6bb4b60); HH(b, c, d, a, x[10], 23, 0xbebfbc70);
    HH(a, b, c, d, x[13],  4, 0x289b7ec6); HH(d, a, b, c, x[ 0], 11, 0xeaa127fa);
    HH(c, d, a, b, x[ 3], 16, 0xd4ef3085); HH(b, c, d, a, x[ 6], 23, 0x04881d05);
    HH(a, b, c, d, x[ 9],  4, 0xd9d4d039); HH(d, a, b, c, x[12], 11, 0xe6db99e5);
    HH(c, d, a, b, x[15], 16, 0x1fa27cf8); HH(b, c, d, a, x[ 2], 23, 0xc4ac5665);

    II(a, b, c, d, x[ 0],  6, 0xf4292244); II(d, a, b, c, x[ 7], 10, 0x432aff97);
    II(c, d, a, b, x[14], 15, 0xab9423a7); II(b, c, d, a, x[ 5], 21, 0xfc93a039);
    II(a, b, c, d, x[12],  6, 0x655b59c3); II(d, a, b, c, x[ 3], 10, 0x8f0ccc92);
    II(c, d, a, b, x[10], 15, 0xffeff47d); II(b, c, d, a, x[ 1], 21, 0x85845dd1);
    II(a, b, c, d, x[ 8],  6, 0x6fa87e4f); II(d, a, b, c, x[15], 10, 0xfe2ce6e0);
    II(c, d, a, b, x[ 6], 15, 0xa3014314); II(b, c, d, a, x[13], 21, 0x4e0811a1);
    II(a, b, c, d, x[ 4],  6, 0xf7537e82); II(d, a, b, c, x[11], 10, 0xbd3af235);
    II(c, d, a, b, x[ 2], 15, 0x2ad7d2bb); II(b, c, d, a, x[ 9], 21, 0xeb86d391);

    state[0] += a; state[1] += b; state[2] += c; state[3] += d;
}

std::string md5(const std::string& message) {
    MD5_u32 state[4] = {
        0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476
    };
    
    size_t original_len = message.length();
    size_t padded_len = original_len + 8;
    padded_len += (64 - (padded_len % 64)) % 64;
    
    std::vector<unsigned char> padded_message(padded_len);
    std::copy(message.begin(), message.end(), padded_message.begin());
    
    padded_message[original_len] = 0x80;
    
    unsigned long long bits_len = original_len * 8;
    for (int i = 0; i < 8; ++i) {
        padded_message[padded_len - 8 + i] = (unsigned char)((bits_len >> (i * 8)) & 0xFF);
    }
    
    for (size_t i = 0; i < padded_len; i += 64) {
        md5_transform(state, &padded_message[i]);
    }

    std::ostringstream ss;
    ss << std::hex << std::setfill('0');
    for (int i = 0; i < 4; ++i) {
        ss << std::setw(2) << static_cast<unsigned int>((state[i] & 0xFF))
           << std::setw(2) << static_cast<unsigned int>((state[i] >> 8) & 0xFF)
           << std::setw(2) << static_cast<unsigned int>((state[i] >> 16) & 0xFF)
           << std::setw(2) << static_cast<unsigned int>((state[i] >> 24) & 0xFF);
    }
    return ss.str();
}

std::string salt;
std::vector<std::string> cached_hashes;

std::string get_hash(int index) {
    if (index < cached_hashes.size()) {
        return cached_hashes[index];
    }
    for (int i = cached_hashes.size(); i <= index; ++i) {
        cached_hashes.push_back(md5(salt + std::to_string(i)));
    }
    return cached_hashes[index];
}

char find_triplet(const std::string& hash_str) {
    for (size_t i = 0; i < hash_str.length() - 2; ++i) {
        if (hash_str[i] == hash_str[i+1] && hash_str[i+1] == hash_str[i+2]) {
            return hash_str[i];
        }
    }
    return '\0';
}

bool has_quintuplet(const std::string& hash_str, char c) {
    std::string quint(5, c);
    return hash_str.find(quint) != std::string::npos;
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::ifstream file("input.txt");
    std::getline(file, salt);
    file.close();

    int keys = 0;
    int index = 0;

    while (keys < 64) {
        char triplet_char = find_triplet(get_hash(index));
        
        if (triplet_char != '\0') {
            for (int i = 1; i <= 1000; ++i) {
                if (has_quintuplet(get_hash(index + i), triplet_char)) {
                    keys++;
                    break;
                }
            }
        }
        index++;
    }

    std::cout << index - 1 << std::endl;

    return 0;
}
