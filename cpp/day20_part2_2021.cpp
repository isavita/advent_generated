
#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <algorithm>

const int iterations = 50;
const int expandBy = 1;

std::pair<std::string, std::vector<std::vector<bool> > > readInput(std::string filename);
std::vector<std::vector<bool> > enhanceImage(std::string algorithm, std::vector<std::vector<bool> > image, bool useInfiniteLit);
int countLitPixels(std::vector<std::vector<bool> > image);

int main() {
    auto input = readInput("input.txt");
    std::string algorithm = input.first;
    std::vector<std::vector<bool> > image = input.second;

    for (int i = 0; i < iterations; i++) {
        image = enhanceImage(algorithm, image, i % 2 == 1 && algorithm[0] == '#');
    }

    std::cout << countLitPixels(image) << std::endl;

    return 0;
}

std::pair<std::string, std::vector<std::vector<bool> > > readInput(std::string filename) {
    std::ifstream file(filename);
    if (!file.is_open()) {
        throw std::runtime_error("Error opening file");
    }

    std::string algorithm;
    std::vector<std::vector<bool> > image;

    std::string line;
    std::getline(file, algorithm);
    std::getline(file, line); // skip the empty line

    while (std::getline(file, line)) {
        line.erase(std::remove(line.begin(), line.end(), ' '), line.end());
        std::vector<bool> row;
        for (char c : line) {
            row.push_back(c == '#');
        }
        image.push_back(row);
    }

    return std::make_pair(algorithm, image);
}

std::vector<std::vector<bool> > enhanceImage(std::string algorithm, std::vector<std::vector<bool> > image, bool useInfiniteLit) {
    int height = image.size();
    int width = image[0].size();

    std::vector<std::vector<bool> > newImage(height + (expandBy * 2), std::vector<bool>(width + (expandBy * 2)));

    for (int y = -expandBy; y < height + expandBy; y++) {
        for (int x = -expandBy; x < width + expandBy; x++) {
            int index = 0;
            for (int dy = -1; dy <= 1; dy++) {
                for (int dx = -1; dx <= 1; dx++) {
                    index <<= 1;
                    int ny = y + dy;
                    int nx = x + dx;
                    if (ny >= 0 && ny < height && nx >= 0 && nx < width) {
                        if (image[ny][nx]) {
                            index |= 1;
                        }
                    } else if (useInfiniteLit) {
                        index |= 1;
                    }
                }
            }
            newImage[y + expandBy][x + expandBy] = algorithm[index] == '#';
        }
    }

    return newImage;
}

int countLitPixels(std::vector<std::vector<bool> > image) {
    int count = 0;
    for (const auto& row : image) {
        for (bool pixel : row) {
            if (pixel) {
                count++;
            }
        }
    }
    return count;
}
