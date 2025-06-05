
#include <fstream>
#include <iostream>
#include <map>
#include <set>
#include <string>
#include <utility>
#include <vector>

using Coord = std::pair<int, int>;
using PipeDirections = std::set<Coord>;

const char EMPTY = '.';
const char START = 'S';
const char VERTICAL = '|';
const char HORIZONTAL = '-';
const char TOP_LEFT_CORNER = 'J';
const char TOP_RIGHT_CORNER = 'L';
const char BOTTOM_LEFT_CORNER = '7';
const char BOTTOM_RIGHT_CORNER = 'F';

const Coord UNDEFINED = {0, 0};
const Coord TOP = {0, -1};
const Coord RIGHT = {1, 0};
const Coord BOTTOM = {0, 1};
const Coord LEFT = {-1, 0};

std::map<char, PipeDirections> TILE_TO_PIPE;

void initTileToPipe() {
    TILE_TO_PIPE[VERTICAL] = {TOP, BOTTOM};
    TILE_TO_PIPE[HORIZONTAL] = {LEFT, RIGHT};
    TILE_TO_PIPE[TOP_LEFT_CORNER] = {TOP, LEFT};
    TILE_TO_PIPE[TOP_RIGHT_CORNER] = {TOP, RIGHT};
    TILE_TO_PIPE[BOTTOM_LEFT_CORNER] = {BOTTOM, LEFT};
    TILE_TO_PIPE[BOTTOM_RIGHT_CORNER] = {BOTTOM, RIGHT};
}

PipeDirections getPipeFromTile(char tile) {
    auto it = TILE_TO_PIPE.find(tile);
    if (it != TILE_TO_PIPE.end()) {
        return it->second;
    }
    return {};
}

std::map<Coord, char> buildGrid(const std::vector<std::string>& inputLines) {
    std::map<Coord, char> grid;
    for (int y = 0; y < inputLines.size(); ++y) {
        for (int x = 0; x < inputLines[y].length(); ++x) {
            char ch = inputLines[y][x];
            if (ch != EMPTY) {
                grid[{x, y}] = ch;
            }
        }
    }
    return grid;
}

Coord findStart(const std::map<Coord, char>& grid) {
    for (const auto& pair : grid) {
        if (pair.second == START) {
            return pair.first;
        }
    }
    return UNDEFINED;
}

PipeDirections getPipeFromNeighbors(Coord coord, const std::map<Coord, char>& grid) {
    PipeDirections pipe;
    std::map<Coord, Coord> possibleNeighborOffsets = {
        {TOP, TOP},
        {RIGHT, RIGHT},
        {BOTTOM, BOTTOM},
        {LEFT, LEFT}};

    for (const auto& entry : possibleNeighborOffsets) {
        Coord dir = entry.first;
        Coord offset = entry.second;
        Coord neighborCoord = {coord.first + offset.first, coord.second + offset.second};

        auto it = grid.find(neighborCoord);
        if (it != grid.end()) {
            char neighborTile = it->second;
            PipeDirections neighborPipe = getPipeFromTile(neighborTile);

            Coord oppositeDir = {-dir.first, -dir.second};
            if (neighborPipe.count(oppositeDir)) {
                pipe.insert(dir);
            }
        }
    }
    return pipe;
}

std::vector<Coord> pathFinding(Coord start, const std::map<Coord, char>& grid) {
    std::vector<Coord> path;
    path.push_back(start);

    PipeDirections startPipe = getPipeFromNeighbors(start, grid);

    Coord previousDir = UNDEFINED;
    Coord current = UNDEFINED;

    if (!startPipe.empty()) {
        previousDir = *startPipe.begin();
        current = {start.first + previousDir.first, start.second + previousDir.second};
    } else {
        return {};
    }

    while (current != start) {
        path.push_back(current);
        char currentTile = grid.at(current);
        PipeDirections currentPipe = getPipeFromTile(currentTile);

        for (const auto& dir : currentPipe) {
            Coord oppositePrevDir = {-previousDir.first, -previousDir.second};
            if (dir != oppositePrevDir) {
                previousDir = dir;
                current = {current.first + dir.first, current.second + dir.second};
                break;
            }
        }
    }
    return path;
}

std::vector<std::string> readFile(const std::string& fileName) {
    std::vector<std::string> lines;
    std::ifstream file(fileName);
    std::string line;
    while (std::getline(file, line)) {
        lines.push_back(line);
    }
    return lines;
}

int solve(const std::vector<std::string>& inputLines) {
    std::map<Coord, char> grid = buildGrid(inputLines);
    Coord start = findStart(grid);
    std::vector<Coord> path = pathFinding(start, grid);
    int numPipesVisited = path.size();
    return numPipesVisited / 2;
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    initTileToPipe();

    std::vector<std::string> input = readFile("input.txt");
    std::cout << solve(input) << std::endl;

    return 0;
}
