
#include <iostream>
#include <fstream>
#include <string>
#include <unordered_set>

int main() {
    std::ifstream file("input.txt");
    std::string directions;
    std::getline(file, directions);
    std::unordered_set<long long> visitedHouses;
    int xSanta = 0, ySanta = 0;
    int xRobo = 0, yRobo = 0;
    bool isSantaTurn = true;
    visitedHouses.insert(0);
    for (char dir : directions) {
        int *x, *y;
        if (isSantaTurn) {
            x = &xSanta;
            y = &ySanta;
        } else {
            x = &xRobo;
            y = &yRobo;
        }
        switch (dir) {
            case '^':
                (*y)++;
                break;
            case 'v':
                (*y)--;
                break;
            case '>':
                (*x)++;
                break;
            case '<':
                (*x)--;
                break;
        }
        long long hash = (static_cast<long long>(*x) << 32) | (static_cast<unsigned int>(*y));
        visitedHouses.insert(hash);
        isSantaTurn = !isSantaTurn;
    }
    std::cout << visitedHouses.size() << std::endl;
    return 0;
}
