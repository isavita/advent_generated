
#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <numeric>
#include <sstream>
#include <cmath>
#include <iomanip>

using int128 = __int128_t;

struct Rat {
    int128 n = 0, d = 1;

    void normalize() {
        if (d == 0) return;
        if (n == 0) {
            d = 1;
            return;
        }
        int128 common = std::gcd(n, d);
        n /= common;
        d /= common;
        if (d < 0) {
            n = -n;
            d = -d;
        }
    }

    Rat(int128 num = 0, int128 den = 1) : n(num), d(den) {
        normalize();
    }
};

Rat operator+(const Rat& a, const Rat& b) { return Rat(a.n * b.d + b.n * a.d, a.d * b.d); }
Rat operator-(const Rat& a, const Rat& b) { return Rat(a.n * b.d - b.n * a.d, a.d * b.d); }
Rat operator*(const Rat& a, const Rat& b) { return Rat(a.n * b.n, a.d * b.d); }
Rat operator/(const Rat& a, const Rat& b) { return Rat(a.n * b.d, a.d * b.n); }
Rat operator-(const Rat& a) { return Rat(-a.n, a.d); }

struct RatVec3 {
    Rat x, y, z;
};

RatVec3 operator+(const RatVec3& a, const RatVec3& b) { return {a.x + b.x, a.y + b.y, a.z + b.z}; }
RatVec3 operator-(const RatVec3& a, const RatVec3& b) { return {a.x - b.x, a.y - b.y, a.z - b.z}; }
RatVec3 operator*(const RatVec3& a, const Rat& s) { return {a.x * s, a.y * s, a.z * s}; }
RatVec3 operator/(const RatVec3& a, const Rat& s) { return {a.x / s, a.y / s, a.z / s}; }

Rat dot(const RatVec3& a, const RatVec3& b) {
    return a.x * b.x + a.y * b.y + a.z * b.z;
}

RatVec3 cross(const RatVec3& a, const RatVec3& b) {
    return {a.y * b.z - a.z * b.y, a.z * b.x - a.x * b.z, a.x * b.y - a.y * b.x};
}

struct Hailstone {
    RatVec3 p, v;
};

Hailstone operator-(const Hailstone& a, const Hailstone& b) {
    return {a.p - b.p, a.v - b.v};
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);
    std::ifstream inputFile("input.txt");
    std::string line;
    std::vector<Hailstone> hailstones;

    for (int i = 0; i < 3 && std::getline(inputFile, line); ++i) {
        std::stringstream ss(line);
        long long px, py, pz, vx, vy, vz;
        char ignore;
        ss >> px >> ignore >> py >> ignore >> pz >> ignore >> vx >> ignore >> vy >> ignore >> vz;
        hailstones.push_back({
            {{(int128)px}, {(int128)py}, {(int128)pz}},
            {{(int128)vx}, {(int128)vy}, {(int128)vz}}
        });
    }

    Hailstone h0 = hailstones[0];
    Hailstone h1 = hailstones[1];
    Hailstone h2 = hailstones[2];

    Hailstone ref1 = h1 - h0;
    Hailstone ref2 = h2 - h0;

    RatVec3 plane2 = cross(ref2.p, ref2.v);
    Rat t1 = -dot(ref1.p, plane2) / dot(ref1.v, plane2);

    RatVec3 plane1 = cross(ref1.p, ref1.v);
    Rat t2 = -dot(ref2.p, plane1) / dot(ref2.v, plane1);

    RatVec3 rock1 = h1.p + h1.v * t1;
    RatVec3 rock2 = h2.p + h2.v * t2;

    RatVec3 v_rock = (rock2 - rock1) / (t2 - t1);
    RatVec3 p_rock = rock1 - v_rock * t1;

    Rat sum = p_rock.x + p_rock.y + p_rock.z;

    long double result_val = static_cast<long double>(sum.n) / static_cast<long double>(sum.d);
    
    std::cout << static_cast<long long>(round(result_val)) << std::endl;

    return 0;
}
