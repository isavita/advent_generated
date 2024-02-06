
#include <iostream>
#include <fstream>
#include <vector>
#include <sstream>

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cout << "Error opening file" << std::endl;
        return 1;
    }

    std::string line;
    std::getline(file, line);
    int earliestDeparture = std::stoi(line);

    std::getline(file, line);
    std::stringstream ss(line);
    std::string id;
    std::vector<int> busIDs;
    while (std::getline(ss, id, ',')) {
        if (id != "x") {
            busIDs.push_back(std::stoi(id));
        }
    }

    int earliestBusID = 0;
    int minWaitTime = earliestDeparture;

    for (int busID : busIDs) {
        int waitTime = busID - (earliestDeparture % busID);
        if (waitTime < minWaitTime) {
            minWaitTime = waitTime;
            earliestBusID = busID;
        }
    }

    std::cout << earliestBusID * minWaitTime << std::endl;

    return 0;
}
