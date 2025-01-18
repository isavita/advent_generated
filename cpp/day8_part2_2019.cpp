
#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <algorithm>

using namespace std;

int main() {
    ifstream inputFile("input.txt");
    if (!inputFile.is_open()) {
        cerr << "Error opening input.txt" << endl;
        return 1;
    }

    string imageData;
    getline(inputFile, imageData);
    imageData.erase(remove(imageData.begin(), imageData.end(), '\n'), imageData.end());

    int width = 25;
    int height = 6;
    int layerSize = width * height;
    vector<char> finalImage(layerSize, '2');

    for (int i = 0; i < imageData.size(); i += layerSize) {
        int end = min((int)imageData.size(), i + layerSize);
        for (int j = 0; j < end - i; ++j) {
            if (finalImage[j] == '2') {
                finalImage[j] = imageData[i + j];
            }
        }
    }

    cout << "Decoded image:" << endl;
    for (int i = 0; i < height; ++i) {
        for (int j = 0; j < width; ++j) {
            char pixel = finalImage[i * width + j];
            if (pixel == '0') {
                cout << " ";
            } else if (pixel == '1') {
                cout << "#";
            }
        }
        cout << endl;
    }

    return 0;
}
