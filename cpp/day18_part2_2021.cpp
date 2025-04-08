
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <algorithm>

class SnailNumber {
public:
    int value;
    SnailNumber* left;
    SnailNumber* right;

    SnailNumber(int value = 0, SnailNumber* left = nullptr, SnailNumber* right = nullptr) : value(value), left(left), right(right) {}

    bool is_regular() const {
        return left == nullptr && right == nullptr;
    }

    SnailNumber* add(SnailNumber* other) {
        SnailNumber* new_number = new SnailNumber(0, this, other);
        return new_number->reduce();
    }

    SnailNumber* reduce() {
        while (true) {
            int left_val = 0, right_val = 0;
            if (explode(0, left_val, right_val)) {
                continue;
            }
            if (!split()) {
                break;
            }
        }
        return this;
    }

    bool explode(int depth, int& left_val, int& right_val) {
        if (is_regular()) {
            return false;
        }

        if (depth == 4) {
            left_val = left->value;
            right_val = right->value;
            value = 0;
            delete left;
            delete right;
            left = nullptr;
            right = nullptr;
            return true;
        }

        if (left->explode(depth + 1, left_val, right_val)) {
            if (right_val > 0 && right != nullptr) {
                right->add_left(right_val);
            }
            right_val = 0;
            return true;
        }

        if (right->explode(depth + 1, left_val, right_val)) {
            if (left_val > 0 && left != nullptr) {
                left->add_right(left_val);
            }
            left_val = 0;
            return true;
        }

        return false;
    }

    void add_left(int value) {
        if (is_regular()) {
            this->value += value;
        }
        else {
            left->add_left(value);
        }
    }

    void add_right(int value) {
        if (is_regular()) {
            this->value += value;
        }
        else {
            right->add_right(value);
        }
    }

    bool split() {
        if (is_regular()) {
            if (value >= 10) {
                left = new SnailNumber(value / 2);
                right = new SnailNumber((value + 1) / 2);
                this->value = 0;
                return true;
            }
            return false;
        }
        return left->split() || right->split();
    }

    long long magnitude() const {
        if (is_regular()) {
            return value;
        }
        return 3 * left->magnitude() + 2 * right->magnitude();
    }

    SnailNumber* deep_copy() const {
        if (is_regular()) {
            return new SnailNumber(value);
        }
        return new SnailNumber(0, left->deep_copy(), right->deep_copy());
    }

    ~SnailNumber() {
        delete left;
        delete right;
    }
};

SnailNumber* parse_snail_number(const std::string& input_str) {
    if (input_str[0] != '[') {
        return new SnailNumber(std::stoi(input_str));
    }

    int balance = 0;
    size_t split_index = 0;
    for (size_t i = 1; i < input_str.length() - 1; ++i) {
        if (input_str[i] == '[') {
            balance += 1;
        }
        else if (input_str[i] == ']') {
            balance -= 1;
        }
        else if (input_str[i] == ',' && balance == 0) {
            split_index = i;
            break;
        }
    }

    SnailNumber* left = parse_snail_number(input_str.substr(1, split_index - 1));
    SnailNumber* right = parse_snail_number(input_str.substr(split_index + 1, input_str.length() - split_index - 2));
    return new SnailNumber(0, left, right);
}

int main() {
    std::ifstream file("input.txt");
    std::string line;
    std::vector<SnailNumber*> snail_numbers;

    if (file.is_open()) {
        while (std::getline(file, line)) {
            snail_numbers.push_back(parse_snail_number(line));
        }
        file.close();
    }
    else {
        std::cerr << "Unable to open file" << std::endl;
        return 1;
    }

    if (snail_numbers.empty()) {
        std::cout << "No snailfish numbers found in the file." << std::endl;
        for (auto& num : snail_numbers) {
            delete num;
        }
        return 0;
    }

    long long largest_magnitude = 0;
    for (size_t i = 0; i < snail_numbers.size(); ++i) {
        for (size_t j = 0; j < snail_numbers.size(); ++j) {
            if (i == j) {
                continue;
            }
            SnailNumber* a = snail_numbers[i]->deep_copy();
            SnailNumber* b = snail_numbers[j]->deep_copy();
            long long sum1 = a->add(b)->magnitude();

            delete a;
            delete b;
            a = snail_numbers[i]->deep_copy();
            b = snail_numbers[j]->deep_copy();
            long long sum2 = b->add(a)->magnitude();
             delete a;
            delete b;

            largest_magnitude = std::max(largest_magnitude, std::max(sum1, sum2));
        }
    }

    std::cout << largest_magnitude << std::endl;

    for (auto& num : snail_numbers) {
        delete num;
    }

    return 0;
}
