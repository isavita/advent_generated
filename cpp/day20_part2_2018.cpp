
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <map>
#include <stack>
#include <algorithm>

struct Point {
    int x, y;

    bool operator<(const Point& other) const {
        if (x != other.x) {
            return x < other.x;
        }
        return y < other.y;
    }
};

std::map<Point, int> buildMap(const std::string& directions) {
    std::stack<std::pair<Point, std::map<Point, int>>> s;
    std::map<Point, int> rooms;
    Point currentRoom = {0, 0};
    std::map<Point, int> doorsPassed;

    doorsPassed[currentRoom] = 0;

    for (size_t i = 1; i < directions.length() - 1; ++i) {
        char dirChar = directions[i];

        if (dirChar == '(') {
            s.push({currentRoom, doorsPassed});
        } else if (dirChar == '|') {
            currentRoom = s.top().first;
            doorsPassed = s.top().second;
        } else if (dirChar == ')') {
            currentRoom = s.top().first;
            doorsPassed = s.top().second;
            s.pop();
        } else {
            int dx = 0, dy = 0;
            if (dirChar == 'N') dy = -1;
            else if (dirChar == 'E') dx = 1;
            else if (dirChar == 'S') dy = 1;
            else if (dirChar == 'W') dx = -1;

            Point newRoom = {currentRoom.x + dx, currentRoom.y + dy};
            int newDoors = doorsPassed[currentRoom] + 1;

            doorsPassed[newRoom] = newDoors;

            if (rooms.find(newRoom) == rooms.end() || newDoors < rooms[newRoom]) {
                rooms[newRoom] = newDoors;
            }
            currentRoom = newRoom;
        }
    }
    return rooms;
}

int main() {
    std::ifstream file("input.txt");
    std::string directions;
    if (!file.is_open()) {
        return 1;
    }
    std::getline(file, directions);
    file.close();

    std::map<Point, int> rooms = buildMap(directions);

    int maxDoors = 0;
    long long roomsWith1000Doors = 0;

    for (const auto& pair : rooms) {
        if (pair.second > maxDoors) {
            maxDoors = pair.second;
        }
        if (pair.second >= 1000) {
            roomsWith1000Doors++;
        }
    }

    std::cout << maxDoors << std::endl;
    std::cout << roomsWith1000Doors << std::endl;

    return 0;
}
