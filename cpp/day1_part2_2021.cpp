#include <fstream>
#include <vector>
#include <iostream>

int main() {
    std::ifstream file("input.txt");
    if (!file) {
        std::cerr << "Error opening file" << std::endl;
        return 1;
    }

    std::vector<int> vals;
    int val;
    while (file >> val) {
        vals.push_back(val);
    }

    if (file.bad()) {
        std::cerr << "Error reading file" << std::endl;
        return 1;
    }

    int prevSum = vals[0] + vals[1] + vals[2];
    int count = 0;
    for (int i = 3; i < vals.size(); i++) {
        int currSum = vals[i-2] + vals[i-1] + vals[i];
        if (currSum > prevSum) {
            count++;
        }
        prevSum = currSum;
    }

    std::cout << count << std::endl;

    return 0;
}