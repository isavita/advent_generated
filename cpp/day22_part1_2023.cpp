
#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>

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

int max(int a, int b) {
    return a > b ? a : b;
}

int min(int a, int b) {
    return a < b ? a : b;
}

std::vector<Brick*> parseInput(std::vector<std::string> input) {
    std::vector<Brick*> bricks(input.size());
    for (int i = 0; i < input.size(); i++) {
        Brick* brick = new Brick();
        brick->basedOn = {};
        brick->support = {};
        sscanf(input[i].c_str(), "%d,%d,%d~%d,%d,%d", &brick->mini.x, &brick->mini.y, &brick->mini.z, &brick->maxi.x, &brick->maxi.y, &brick->maxi.z);
        bricks[i] = brick;
    }
    return bricks;
}

void settle(std::vector<Brick*>& bricks) {
    std::sort(bricks.begin(), bricks.end(), [](Brick* a, Brick* b) {
        return a->maxi.z < b->maxi.z;
    });

    for (int i = 0; i < bricks.size(); i++) {
        int supportZ = 0;
        std::vector<Brick*> basedBricks;

        for (int j = i - 1; j > -1; j--) {
            bool isIntersectingX = max(bricks[i]->mini.x, bricks[j]->mini.x) <= min(bricks[i]->maxi.x, bricks[j]->maxi.x);
            bool isIntersectingY = max(bricks[i]->mini.y, bricks[j]->mini.y) <= min(bricks[i]->maxi.y, bricks[j]->maxi.y);
            bool isIntersecting = isIntersectingX && isIntersectingY;
            if (isIntersecting) {
                if (bricks[j]->maxi.z == supportZ) {
                    basedBricks.push_back(bricks[j]);
                } else if (bricks[j]->maxi.z > supportZ) {
                    supportZ = bricks[j]->maxi.z;
                    basedBricks = {bricks[j]};
                }
            }
        }

        bricks[i]->basedOn = basedBricks;
        for (Brick* basedBrick : basedBricks) {
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
    for (Brick* brick : bricks) {
        bool isDisintegratable = true;
        for (Brick* supportedBrick : brick->support) {
            if (supportedBrick->basedOn.size() < 2) {
                isDisintegratable = false;
                break;
            }
        }
        if (isDisintegratable) {
            cnt++;
        }
    }
    return cnt;
}

std::vector<std::string> readFile(std::string fileName) {
    std::ifstream file(fileName);
    std::vector<std::string> input;
    std::string line;
    while (std::getline(file, line)) {
        input.push_back(line);
    }
    return input;
}

int main() {
    std::vector<std::string> input = readFile("input.txt");
    std::cout << solve(input) << std::endl;
    return 0;
}
