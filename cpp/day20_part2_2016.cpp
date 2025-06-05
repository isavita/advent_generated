
#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <algorithm> // For std::sort and std::max
#include <utility>   // For std::pair

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::ifstream inputFile("input.txt");
    
    std::vector<std::pair<unsigned int, unsigned int>> data;
    std::string line;

    while (std::getline(inputFile, line)) {
        size_t dashPos = line.find('-');
        unsigned int start = std::stoul(line.substr(0, dashPos));
        unsigned int end = std::stoul(line.substr(dashPos + 1));
        data.push_back({start, end});
    }
    inputFile.close();

    std::sort(data.begin(), data.end());

    // current_max_blocked represents the highest IP that is currently blocked or covered by a previous range.
    // Initialized to 0, mirroring the Python script's `current_max = 0`.
    unsigned long long current_max_blocked = 0; 

    unsigned long long allowed_ips = 0;
    const unsigned int MAX_IP = 4294967295U; // Maximum 32-bit unsigned integer value (2^32 - 1)

    for (const auto& range : data) {
        unsigned int start = range.first;
        unsigned int end = range.second;

        // If the start of the current block is greater than the current highest blocked IP,
        // it means there's a gap of allowed IPs.
        // The IPs in this gap are from (current_max_blocked + 1) to (start - 1).
        // The number of IPs is (start - 1) - (current_max_blocked + 1) + 1,
        // which simplifies to start - current_max_blocked - 1.
        // This calculation is safe because `start` is promoted to `unsigned long long`
        // before subtraction with `current_max_blocked`.
        // If `current_max_blocked` is already `MAX_IP`, then `start > current_max_blocked`
        // will be false (as `start` cannot exceed `MAX_IP`), so no more IPs are added.
        if (start > current_max_blocked) {
            allowed_ips += start - current_max_blocked - 1;
        }

        // Update current_max_blocked to the maximum of its current value and the end of the current range.
        // This effectively extends the contiguous blocked range if the current block goes further.
        current_max_blocked = std::max(current_max_blocked, (unsigned long long)end);
    }

    // After processing all blocks, add any remaining allowed IPs.
    // These IPs are from (current_max_blocked + 1) up to MAX_IP.
    // The number of IPs is MAX_IP - (current_max_blocked + 1) + 1, which simplifies to MAX_IP - current_max_blocked.
    // This applies if the last block didn't cover the entire IP range up to MAX_IP.
    // This is safe as `current_max_blocked` will be at most `MAX_IP`.
    if (current_max_blocked < MAX_IP) {
        allowed_ips += MAX_IP - current_max_blocked;
    }

    std::cout << allowed_ips << std::endl;

    return 0;
}
