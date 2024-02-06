
#include <iostream>
#include <fstream>
#include <string>
#include <regex>
#include <vector>
#include <climits>

struct Star {
    int x;
    int y;
    int vX;
    int vY;
    Star* next;
};

int toInt(std::string s) {
    return std::stoi(s);
}

int main() {
    std::ifstream input("input.txt");
    std::string line;
    std::vector<Star*> stars;
    Star* head = new Star();
    Star* tail = head;
    std::regex re("position=<\\s*(-?\\d+),\\s*(-?\\d+)> velocity=<\\s*(-?\\d+),\\s*(-?\\d+)>");
    
    while (std::getline(input, line)) {
        std::smatch match;
        if (std::regex_search(line, match, re)) {
            Star* star = new Star{
                toInt(match[1]),
                toInt(match[2]),
                toInt(match[3]),
                toInt(match[4]),
                nullptr
            };
            tail->next = star;
            tail = star;
        }
    }

    int smallestT = 0;
    int smallestArea = INT_MAX;
    for (int t = 1; t < 100000; t++) {
        int maxX = 0;
        int maxY = 0;
        int minX = 0;
        int minY = 0;

        for (Star* temp = head->next; temp->next != nullptr; temp = temp->next) {
            int x = temp->x + temp->vX * t;
            if (maxX < x) {
                maxX = x;
            } else if (minX > x) {
                minX = x;
            }
            int y = temp->y + temp->vY * t;
            if (maxY < y) {
                maxY = y;
            } else if (minY > y) {
                minY = y;
            }
        }

        int lenX = maxX - minY + 1;
        int lenY = maxY - minY + 1;
        int area = lenX + lenY;

        if (smallestArea > area) {
            smallestArea = area;
            smallestT = t;
        }
    }
    std::cout << smallestT << std::endl;

    int t = smallestT;

    int maxX = 0;
    int maxY = 0;
    int minX = 0;
    int minY = 0;

    for (Star* temp = head->next; temp->next != nullptr; temp = temp->next) {
        temp->x = temp->x + temp->vX * t;
        if (maxX < temp->x) {
            maxX = temp->x;
        } else if (minX > temp->x) {
            minX = temp->x;
        }
        temp->y = temp->y + temp->vY * t;
        if (maxY < temp->y) {
            maxY = temp->y;
        } else if (minY > temp->y) {
            minY = temp->y;
        }
    }

    std::vector<std::vector<bool>> mapper(maxY - minY + 1, std::vector<bool>(maxX - minX + 1, false));

    for (Star* temp = head->next; temp->next != nullptr; temp = temp->next) {
        mapper[temp->y][temp->x] = true;
    }

    for (int i = 0; i < mapper.size(); i++) {
        for (int j = 0; j < mapper[0].size(); j++) {
            // Do something if needed
        }
    }

    return 0;
}
