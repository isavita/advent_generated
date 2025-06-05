
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <algorithm>
#include <map>

struct LogEntry {
    std::string timestamp;
    std::string action;

    bool operator<(const LogEntry& other) const {
        return timestamp < other.timestamp;
    }
};

int main() {
    std::ifstream inputFile("input.txt");
    std::vector<LogEntry> records;
    std::string line;

    while (std::getline(inputFile, line)) {
        std::string timestamp = line.substr(1, 16);
        std::string action = line.substr(19);
        records.push_back({timestamp, action});
    }

    inputFile.close();

    std::sort(records.begin(), records.end());

    int currentGuardId = -1;
    int asleepMinute = -1;

    std::map<int, int> guardsTotalSleep;
    std::map<int, std::map<int, int>> guardsMinuteCounts;

    for (const auto& record : records) {
        if (record.action.find("Guard") != std::string::npos) {
            size_t hashPos = record.action.find('#');
            size_t spacePos = record.action.find(' ', hashPos);
            currentGuardId = std::stoi(record.action.substr(hashPos + 1, spacePos - (hashPos + 1)));
        } else if (record.action.find("falls asleep") != std::string::npos) {
            asleepMinute = std::stoi(record.timestamp.substr(14, 2));
        } else if (record.action.find("wakes up") != std::string::npos) {
            int wakeUpMinute = std::stoi(record.timestamp.substr(14, 2));
            for (int minute = asleepMinute; minute < wakeUpMinute; ++minute) {
                guardsTotalSleep[currentGuardId]++;
                guardsMinuteCounts[currentGuardId][minute]++;
            }
        }
    }

    int mostAsleepGuard = -1;
    int maxTotalSleep = 0;

    for (const auto& pair : guardsTotalSleep) {
        if (pair.second > maxTotalSleep) {
            maxTotalSleep = pair.second;
            mostAsleepGuard = pair.first;
        }
    }

    int mostAsleepMinute = -1;
    int maxMinuteCount = 0;

    if (guardsMinuteCounts.count(mostAsleepGuard)) {
        for (const auto& pair : guardsMinuteCounts[mostAsleepGuard]) {
            if (pair.second > maxMinuteCount) {
                maxMinuteCount = pair.second;
                mostAsleepMinute = pair.first;
            }
        }
    }

    std::cout << mostAsleepGuard * mostAsleepMinute << std::endl;

    return 0;
}
