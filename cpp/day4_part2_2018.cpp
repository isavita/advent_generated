
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <map>
#include <algorithm>
#include <numeric>

int main() {
    std::ifstream file("input.txt");
    std::vector<std::string> lines;
    std::string line;

    while (std::getline(file, line)) {
        lines.push_back(line);
    }

    std::sort(lines.begin(), lines.end());

    std::map<int, std::vector<int>> guards;
    int current_guard = 0;
    int falls_asleep_minute = 0;

    for (const std::string& l : lines) {
        int minute = std::stoi(l.substr(15, 2));

        if (l.find("Guard #") != std::string::npos) {
            size_t hash_pos = l.find('#');
            size_t space_pos = l.find(' ', hash_pos);
            current_guard = std::stoi(l.substr(hash_pos + 1, space_pos - (hash_pos + 1)));
            if (guards.find(current_guard) == guards.end()) {
                guards[current_guard] = std::vector<int>(60, 0);
            }
        } else if (l.find("falls asleep") != std::string::npos) {
            falls_asleep_minute = minute;
        } else if (l.find("wakes up") != std::string::npos) {
            int wakes_up_minute = minute;
            for (int i = falls_asleep_minute; i < wakes_up_minute; ++i) {
                guards[current_guard][i]++;
            }
        }
    }

    int sleepiest_guard_id = 0;
    long long max_total_sleep = 0;

    for (const auto& pair : guards) {
        int guard_id = pair.first;
        const std::vector<int>& minutes = pair.second;
        long long current_total_sleep = std::accumulate(minutes.begin(), minutes.end(), 0LL);

        if (current_total_sleep > max_total_sleep) {
            max_total_sleep = current_total_sleep;
            sleepiest_guard_id = guard_id;
        }
    }

    int most_common_minute_part1 = 0;
    if (sleepiest_guard_id != 0) {
        const std::vector<int>& sleepiest_guard_minutes = guards[sleepiest_guard_id];
        auto it = std::max_element(sleepiest_guard_minutes.begin(), sleepiest_guard_minutes.end());
        most_common_minute_part1 = std::distance(sleepiest_guard_minutes.begin(), it);
    }

    long long result_part1 = (long long)sleepiest_guard_id * most_common_minute_part1;
    std::cout << result_part1 << std::endl;

    int guard_id_part2 = 0;
    int most_frequent_minute_part2 = 0;
    int max_minute_frequency_part2 = 0;

    for (const auto& pair : guards) {
        int guard_id = pair.first;
        const std::vector<int>& minutes = pair.second;

        auto it = std::max_element(minutes.begin(), minutes.end());
        int current_max_freq = *it;
        int current_minute = std::distance(minutes.begin(), it);

        if (current_max_freq > max_minute_frequency_part2) {
            max_minute_frequency_part2 = current_max_freq;
            guard_id_part2 = guard_id;
            most_frequent_minute_part2 = current_minute;
        }
    }

    long long result_part2 = (long long)guard_id_part2 * most_frequent_minute_part2;
    std::cout << result_part2 << std::endl;

    return 0;
}
