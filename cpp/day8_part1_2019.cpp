#include <iostream>
#include <fstream>
#include <string>

int main() {
    std::ifstream inputFile("input.txt");
    std::string imageData;
    std::getline(inputFile, imageData);

    int width = 25, height = 6;
    int layerSize = width * height;

    int minZeros = layerSize + 1;
    int result = 0;

    for (int i = 0; i < imageData.length(); i += layerSize) {
        std::string layer = imageData.substr(i, std::min(layerSize, static_cast<int>(imageData.length()) - i));
        int zeroCount = 0, oneCount = 0, twoCount = 0;

        for (char pixel : layer) {
            switch (pixel) {
                case '0':
                    zeroCount++;
                    break;
                case '1':
                    oneCount++;
                    break;
                case '2':
                    twoCount++;
                    break;
            }
        }

        if (zeroCount < minZeros) {
            minZeros = zeroCount;
            result = oneCount * twoCount;
        }
    }

    std::cout << result << std::endl;

    return 0;
}