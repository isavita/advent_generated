
#include <iostream>
#include <fstream>
#include <vector>
#include <string>

std::pair<std::string, std::vector<std::vector<char> > > readInput(std::string filename);
std::vector<std::vector<char> > enhanceImage(std::vector<std::vector<char> >& image, std::string algorithm, int times);
std::vector<std::vector<char> > applyAlgorithm(std::vector<std::vector<char> >& image, std::string algorithm, bool flip);
int calculateIndex(int i, int j, std::vector<std::vector<char> >& image, bool flip);
int countLitPixels(std::vector<std::vector<char> >& image);

int main() {
    auto input = readInput("input.txt");
    std::string algorithm = input.first;
    std::vector<std::vector<char> > image = input.second;
    image = enhanceImage(image, algorithm, 2);
    std::cout << countLitPixels(image) << std::endl;
    return 0;
}

std::pair<std::string, std::vector<std::vector<char> > > readInput(std::string filename) {
    std::ifstream file(filename);
    if (!file.is_open()) {
        std::cerr << "Error opening file" << std::endl;
        exit(1);
    }

    std::string algorithm;
    std::getline(file, algorithm);
    algorithm.erase(std::remove(algorithm.begin(), algorithm.end(), '\n'), algorithm.end());
    file.ignore();

    std::vector<std::vector<char> > image;
    std::string line;
    while (std::getline(file, line)) {
        image.push_back(std::vector<char>(line.begin(), line.end()));
    }

    return {algorithm, image};
}

std::vector<std::vector<char> > enhanceImage(std::vector<std::vector<char> >& image, std::string algorithm, int times) {
    for (int i = 0; i < times; i++) {
        image = applyAlgorithm(image, algorithm, i % 2 == 1 && algorithm[0] == '#');
    }
    return image;
}

std::vector<std::vector<char> > applyAlgorithm(std::vector<std::vector<char> >& image, std::string algorithm, bool flip) {
    std::vector<std::vector<char> > enhancedImage(image.size() + 2, std::vector<char>(image[0].size() + 2));
    for (int i = 0; i < enhancedImage.size(); i++) {
        for (int j = 0; j < enhancedImage[i].size(); j++) {
            int index = calculateIndex(i - 1, j - 1, image, flip);
            enhancedImage[i][j] = algorithm[index];
        }
    }
    return enhancedImage;
}

int calculateIndex(int i, int j, std::vector<std::vector<char> >& image, bool flip) {
    int index = 0;
    for (int di = -1; di <= 1; di++) {
        for (int dj = -1; dj <= 1; dj++) {
            index <<= 1;
            if (i + di >= 0 && i + di < image.size() && j + dj >= 0 && j + dj < image[0].size()) {
                if (image[i + di][j + dj] == '#') {
                    index |= 1;
                }
            } else if (flip) {
                index |= 1;
            }
        }
    }
    return index;
}

int countLitPixels(std::vector<std::vector<char> >& image) {
    int count = 0;
    for (const auto& row : image) {
        for (char pixel : row) {
            if (pixel == '#') {
                count++;
            }
        }
    }
    return count;
}
