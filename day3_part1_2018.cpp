#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <unordered_map>

struct Claim {
    int id;
    int left;
    int top;
    int width;
    int height;
};

Claim ParseClaim(std::string s) {
    Claim c;
    std::sscanf(s.c_str(), "#%d @ %d,%d: %dx%d", &c.id, &c.left, &c.top, &c.width, &c.height);
    return c;
}

std::vector<Claim> ReadClaims(std::string filename) {
    std::ifstream file(filename);
    std::vector<Claim> claims;
    std::string line;
    while (std::getline(file, line)) {
        Claim claim = ParseClaim(line);
        claims.push_back(claim);
    }
    return claims;
}

int CountOverlappingInches(std::vector<Claim> claims) {
    std::unordered_map<std::string, int> fabric;
    for (const auto& claim : claims) {
        for (int i = claim.left; i < claim.left + claim.width; i++) {
            for (int j = claim.top; j < claim.top + claim.height; j++) {
                std::string coord = std::to_string(i) + "," + std::to_string(j);
                fabric[coord]++;
            }
        }
    }

    int overlapping = 0;
    for (const auto& count : fabric) {
        if (count.second > 1) {
            overlapping++;
        }
    }
    return overlapping;
}

int main() {
    std::vector<Claim> claims = ReadClaims("input.txt");
    int overlapping = CountOverlappingInches(claims);
    std::cout << overlapping << std::endl;
    return 0;
}