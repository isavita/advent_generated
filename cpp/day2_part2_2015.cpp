#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <sstream>
#include <algorithm>

int main() {
    std::ifstream file("input.txt");
    std::string line;
    int totalPaper = 0;
    int totalRibbon = 0;

    while (std::getline(file, line)) {
        std::stringstream ss(line);
        std::vector<int> dimensions;
        std::string token;
        
        while (std::getline(ss, token, 'x')) {
            dimensions.push_back(std::stoi(token));
        }

        std::sort(dimensions.begin(), dimensions.end());

        int l = dimensions[0];
        int w = dimensions[1];
        int h = dimensions[2];

        int paper = 2*l*w + 2*w*h + 2*h*l + l*w;
        totalPaper += paper;

        int ribbon = 2*l + 2*w + l*w*h;
        totalRibbon += ribbon;
    }

    std::cout << totalPaper << std::endl;
    std::cout << totalRibbon << std::endl;

    return 0;
}