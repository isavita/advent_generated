
#include <iostream>
#include <fstream>
#include <vector>
#include <sstream>

struct Claim {
    int ID;
    int X;
    int Y;
    int Width;
    int Height;
};

std::vector<Claim> readClaims(const std::string& filename) {
    std::ifstream file(filename);
    std::vector<Claim> claims;

    std::string line;
    while (std::getline(file, line)) {
        std::stringstream ss(line);
        std::string token;
        std::vector<std::string> parts;
        while (std::getline(ss, token, ' ')) {
            parts.push_back(token);
        }

        int id = std::stoi(parts[0].substr(1));
        std::vector<std::string> coords;
        std::stringstream coordStream(parts[2].substr(0, parts[2].size() - 1));
        while (std::getline(coordStream, token, ',')) {
            coords.push_back(token);
        }
        int x = std::stoi(coords[0]);
        int y = std::stoi(coords[1]);

        std::vector<std::string> dims;
        std::stringstream dimStream(parts[3]);
        while (std::getline(dimStream, token, 'x')) {
            dims.push_back(token);
        }
        int width = std::stoi(dims[0]);
        int height = std::stoi(dims[1]);

        claims.push_back(Claim{id, x, y, width, height});
    }

    return claims;
}

int main() {
    std::vector<Claim> claims = readClaims("input.txt");

    std::vector<std::vector<int>> fabric(1000, std::vector<int>(1000, 0));

    for (const auto& claim : claims) {
        for (int y = claim.Y; y < claim.Y + claim.Height; y++) {
            for (int x = claim.X; x < claim.X + claim.Width; x++) {
                fabric[y][x]++;
            }
        }
    }

    for (const auto& claim : claims) {
        bool overlap = false;
        for (int y = claim.Y; y < claim.Y + claim.Height; y++) {
            for (int x = claim.X; x < claim.X + claim.Width; x++) {
                if (fabric[y][x] > 1) {
                    overlap = true;
                    break;
                }
            }
            if (overlap) {
                break;
            }
        }

        if (!overlap) {
            std::cout << claim.ID << std::endl;
            return 0;
        }
    }

    return 0;
}
