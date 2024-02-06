
#include <iostream>
#include <fstream>
#include <algorithm>
#include <vector>
#include <unordered_map>

struct Coord {
    int x;
    int y;
    int z;
};

struct Brick {
    Coord mini;
    Coord maxi;
    std::vector<Brick*> basedOn;
    std::vector<Brick*> support;
};

bool compareBricks(const Brick* a, const Brick* b) {
    return a->maxi.z < b->maxi.z;
}

int max(int a, int b) {
    return a > b ? a : b;
}

int min(int a, int b) {
    return a < b ? a : b;
}

std::vector<std::string> readFile(std::string fileName) {
    std::ifstream file(fileName);
    std::vector<std::string> lines;
    std::string line;
    while (std::getline(file, line)) {
        lines.push_back(line);
    }
    return lines;
}

std::vector<Brick*> parseInput(std::vector<std::string> input) {
    std::vector<Brick*> bricks;
    for (const auto& line : input) {
        Brick* brick = new Brick();
        brick->basedOn = std::vector<Brick*>();
        brick->support = std::vector<Brick*>();
        sscanf(line.c_str(), "%d,%d,%d~%d,%d,%d", &brick->mini.x, &brick->mini.y, &brick->mini.z, &brick->maxi.x, &brick->maxi.y, &brick->maxi.z);
        bricks.push_back(brick);
    }
    return bricks;
}

void settle(std::vector<Brick*>& bricks) {
    std::sort(bricks.begin(), bricks.end(), compareBricks);

    for (size_t i = 0; i < bricks.size(); i++) {
        int supportZ = 0;
        std::vector<Brick*> basedBricks;

        for (int j = i - 1; j >= 0; j--) {
            bool isIntersectingX = max(bricks[i]->mini.x, bricks[j]->mini.x) <= min(bricks[i]->maxi.x, bricks[j]->maxi.x);
            bool isIntersectingY = max(bricks[i]->mini.y, bricks[j]->mini.y) <= min(bricks[i]->maxi.y, bricks[j]->maxi.y);
            bool isIntersecting = isIntersectingX && isIntersectingY;
            if (isIntersecting) {
                if (bricks[j]->maxi.z == supportZ) {
                    basedBricks.push_back(bricks[j]);
                } else if (bricks[j]->maxi.z > supportZ) {
                    supportZ = bricks[j]->maxi.z;
                    basedBricks = std::vector<Brick*>{bricks[j]};
                }
            }
        }

        bricks[i]->basedOn = basedBricks;
        for (auto basedBrick : basedBricks) {
            basedBrick->support.push_back(bricks[i]);
        }

        int deltaZ = bricks[i]->maxi.z - bricks[i]->mini.z;
        bricks[i]->mini.z = supportZ + 1;
        bricks[i]->maxi.z = bricks[i]->mini.z + deltaZ;
    }
}

int solve(std::vector<std::string> input) {
    std::vector<Brick*> bricks = parseInput(input);
    settle(bricks);

    int cnt = 0;
    for (auto brick : bricks) {
        std::unordered_map<Brick*, bool> fallingBricks;
        for (auto supportedBrick : brick->support) {
            if (supportedBrick->basedOn.size() == 1) {
                std::vector<Brick*> allSupportedBricks = {supportedBrick};
                while (!allSupportedBricks.empty()) {
                    Brick* supportedBrick0 = allSupportedBricks[0];
                    allSupportedBricks.erase(allSupportedBricks.begin());

                    bool isFalling = true;
                    for (auto basedBrick : supportedBrick0->basedOn) {
                        if (fallingBricks.find(basedBrick) == fallingBricks.end() && basedBrick != brick) {
                            isFalling = false;
                            break;
                        }
                    }

                    if (isFalling) {
                        fallingBricks[supportedBrick0] = true;
                        allSupportedBricks.insert(allSupportedBricks.end(), supportedBrick0->support.begin(), supportedBrick0->support.end());
                    }
                }
            }
        }
        cnt += fallingBricks.size();
    }
    return cnt;
}

int main() {
    std::vector<std::string> input = readFile("input.txt");
    std::cout << solve(input) << std::endl;
    return 0;
}
