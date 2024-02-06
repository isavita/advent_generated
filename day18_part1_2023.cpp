
#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <sstream>

struct Coord {
    int X;
    int Y;

    Coord Add(Coord c2) {
        return {X + c2.X, Y + c2.Y};
    }

    Coord MultiplyByScalar(int s) {
        return {X * s, Y * s};
    }
};

const Coord North = {0, -1};
const Coord West = {-1, 0};
const Coord South = {0, 1};
const Coord East = {1, 0};

int Abs(int x) {
    if (x < 0) {
        return -x;
    }
    return x;
}

std::vector<Coord> parseInput(std::vector<std::string> input) {
    const char Up = 'U';
    const char Left = 'L';
    const char Down = 'D';
    const char Right = 'R';

    Coord current = {0, 0};
    std::vector<Coord> vertices = {current};

    for (const std::string& line : input) {
        std::stringstream ss(line);
        char dirInput;
        std::string lengthStr;
        ss >> dirInput >> lengthStr;

        int length = std::stoi(lengthStr);

        Coord dir;
        switch (dirInput) {
            case Up:
                dir = North;
                break;
            case Left:
                dir = West;
                break;
            case Down:
                dir = South;
                break;
            case Right:
                dir = East;
                break;
        }

        current = current.Add(dir.MultiplyByScalar(length));
        vertices.push_back(current);
    }

    return vertices;
}

int shoelace(std::vector<Coord>& vertices) {
    int n = vertices.size();
    int area = 0;

    for (int i = 0; i < n; i++) {
        int next = (i + 1) % n;
        area += vertices[i].X * vertices[next].Y;
        area -= vertices[i].Y * vertices[next].X;
    }

    area = Abs(area) / 2;
    return area;
}

int perimeter(std::vector<Coord>& vertices) {
    int n = vertices.size();
    int perim = 0;

    for (int i = 0; i < n; i++) {
        int next = (i + 1) % n;
        perim += Abs(vertices[i].X - vertices[next].X) + Abs(vertices[i].Y - vertices[next].Y);
    }

    return perim;
}

int calculatePolygonArea(std::vector<Coord>& vertices) {
    return shoelace(vertices) + perimeter(vertices) / 2 + 1;
}

int solve(std::vector<std::string>& input) {
    std::vector<Coord> vertices = parseInput(input);
    return calculatePolygonArea(vertices);
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

int main() {
    std::vector<std::string> input = readFile("input.txt");
    std::cout << solve(input) << std::endl;
    return 0;
}
