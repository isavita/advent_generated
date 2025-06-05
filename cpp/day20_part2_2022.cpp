
#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <utility>
#include <numeric>

std::vector<long long> readInput() {
    std::vector<long long> numbers;
    std::ifstream file("input.txt");
    std::string line;
    while (std::getline(file, line)) {
        numbers.push_back(std::stoll(line));
    }
    return numbers;
}

void mix(std::vector<std::pair<int, long long>>& nums) {
    int N = nums.size();
    long long n_modulo = N - 1;

    for (int i = 0; i < N; ++i) {
        int old_pos = nums[i].first;
        long long val = nums[i].second;

        if (val == 0) {
            continue;
        }

        long long target_pos_long = (long long)old_pos + val;
        
        int new_pos = (int)((target_pos_long % n_modulo + n_modulo) % n_modulo);
        
        if (old_pos < new_pos) {
            for (int j = 0; j < N; ++j) {
                if (nums[j].first > old_pos && nums[j].first <= new_pos) {
                    nums[j].first--;
                }
            }
        } else if (new_pos < old_pos) {
            for (int j = 0; j < N; ++j) {
                if (nums[j].first >= new_pos && nums[j].first < old_pos) {
                    nums[j].first++;
                }
            }
        }
        nums[i].first = new_pos;
    }
}

long long coords(const std::vector<std::pair<int, long long>>& nums) {
    int N = nums.size();
    int zero_pos = -1;

    for (int i = 0; i < N; ++i) {
        if (nums[i].second == 0) {
            zero_pos = nums[i].first;
            break;
        }
    }

    long long total_sum = 0;
    for (int i = 0; i < N; ++i) {
        if (nums[i].first == (zero_pos + 1000) % N ||
            nums[i].first == (zero_pos + 2000) % N ||
            nums[i].first == (zero_pos + 3000) % N) {
            total_sum += nums[i].second;
        }
    }
    return total_sum;
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::vector<long long> initial_values = readInput();
    
    std::vector<std::pair<int, long long>> nums2(initial_values.size());
    for (int i = 0; i < initial_values.size(); ++i) {
        nums2[i] = {i, 811589153LL * initial_values[i]};
    }

    for (int i = 0; i < 10; ++i) {
        mix(nums2);
    }

    std::cout << coords(nums2) << std::endl;

    return 0;
}

