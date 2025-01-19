
#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <sstream>
#include <algorithm>

using namespace std;

long long concat(long long a, long long b) {
    string sa = to_string(a);
    string sb = to_string(b);
    return stoll(sa + sb);
}

bool canProduce(long long target, const vector<long long>& nums, int idx, long long value) {
    if (idx == nums.size()) {
        return value == target;
    }
    long long n = nums[idx];
    if (canProduce(target, nums, idx + 1, value + n)) {
        return true;
    }
    if (canProduce(target, nums, idx + 1, value * n)) {
        return true;
    }
    if (canProduce(target, nums, idx + 1, concat(value, n))) {
        return true;
    }
    return false;
}

int main() {
    ifstream f("input.txt");
    long long total = 0;
    string line;
    while (getline(f, line)) {
        if (line.empty()) {
            continue;
        }
        size_t pos = line.find(':');
        long long target = stoll(line.substr(0, pos));
        stringstream ss(line.substr(pos + 1));
        vector<long long> nums;
        long long num;
        while (ss >> num) {
            nums.push_back(num);
        }

        if (nums.size() == 1) {
            if (nums[0] == target) {
                total += target;
            }
            continue;
        }
        
        if (canProduce(target, nums, 1, nums[0]))
        {
            total += target;
        }
    }
    cout << total << endl;
    return 0;
}
