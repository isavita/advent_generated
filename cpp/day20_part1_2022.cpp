
#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <sstream>

struct num {
    int pos;
    int val;
};

std::vector<num> readInput(std::string filename) {
    std::ifstream file(filename);
    std::vector<num> nums;
    std::string line;
    int i = 0;
    while (std::getline(file, line)) {
        num n;
        n.pos = i;
        n.val = std::stoi(line);
        nums.push_back(n);
        i++;
    }
    return nums;
}

void mix(std::vector<num>& nums) {
    int n = nums.size() - 1;
    for (size_t i = 0; i < nums.size(); i++) {
        int oldpos = nums[i].pos;
        int newpos = ((oldpos + nums[i].val) % n + n) % n;
        if (oldpos < newpos) {
            for (size_t j = 0; j < nums.size(); j++) {
                if (nums[j].pos > oldpos && nums[j].pos <= newpos) {
                    nums[j].pos--;
                }
            }
        }
        if (newpos < oldpos) {
            for (size_t j = 0; j < nums.size(); j++) {
                if (nums[j].pos >= newpos && nums[j].pos < oldpos) {
                    nums[j].pos++;
                }
            }
        }
        nums[i].pos = newpos;
    }
}

int coords(std::vector<num>& nums) {
    int l = nums.size();
    int zeroPos = 0;
    for (size_t i = 0; i < nums.size(); i++) {
        if (nums[i].val == 0) {
            zeroPos = nums[i].pos;
            break;
        }
    }
    int sum = 0;
    for (size_t i = 0; i < nums.size(); i++) {
        if (nums[i].pos == (zeroPos + 1000) % l || nums[i].pos == (zeroPos + 2000) % l || nums[i].pos == (zeroPos + 3000) % l) {
            sum += nums[i].val;
        }
    }
    return sum;
}

int main() {
    std::vector<num> nums = readInput("input.txt");
    std::vector<num> nums2(nums.size());
    for (size_t i = 0; i < nums.size(); i++) {
        nums2[i].pos = nums[i].pos;
        nums2[i].val = 811589153 * nums[i].val;
    }

    mix(nums);
    std::cout << coords(nums) << std::endl;

    return 0;
}
