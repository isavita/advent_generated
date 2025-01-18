
#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <sstream>
#include <cmath>

struct SnailfishNumber {
    int value;
    SnailfishNumber* left;
    SnailfishNumber* right;
    SnailfishNumber* parent;

    SnailfishNumber(int val) : value(val), left(nullptr), right(nullptr), parent(nullptr) {}
    SnailfishNumber(SnailfishNumber* l, SnailfishNumber* r) : value(-1), left(l), right(r), parent(nullptr) {
        if (left) left->parent = this;
        if (right) right->parent = this;
    }
};

SnailfishNumber* parseSnailfishNumber(const std::string& str, size_t& index) {
    if (str[index] == '[') {
        index++;
        SnailfishNumber* left = parseSnailfishNumber(str, index);
        index++; // comma
        SnailfishNumber* right = parseSnailfishNumber(str, index);
        index++; // closing bracket
        return new SnailfishNumber(left, right);
    } else {
        int value = 0;
        while (isdigit(str[index])) {
            value = value * 10 + (str[index] - '0');
            index++;
        }
        return new SnailfishNumber(value);
    }
}

void addToLeft(SnailfishNumber* node, int value) {
    while (node && node->parent && node->parent->left == node) {
        node = node->parent;
    }
    if (node && node->parent) {
        node = node->parent->left;
        while (node->right) {
            node = node->right;
        }
        node->value += value;
    }
}

void addToRight(SnailfishNumber* node, int value) {
    while (node && node->parent && node->parent->right == node) {
        node = node->parent;
    }
    if (node && node->parent) {
        node = node->parent->right;
        while (node->left) {
            node = node->left;
        }
        node->value += value;
    }
}

bool explode(SnailfishNumber* node, int depth) {
    if (!node) return false;
    if (depth == 4 && node->left && node->right) {
        addToLeft(node, node->left->value);
        addToRight(node, node->right->value);
        delete node->left;
        delete node->right;
        node->left = nullptr;
        node->right = nullptr;
        node->value = 0;
        return true;
    }
    return explode(node->left, depth + 1) || explode(node->right, depth + 1);
}

bool split(SnailfishNumber* node) {
    if (!node) return false;
    if (node->value >= 10) {
        node->left = new SnailfishNumber(node->value / 2);
        node->right = new SnailfishNumber((node->value + 1) / 2);
        node->left->parent = node;
        node->right->parent = node;
        node->value = -1;
        return true;
    }
    return split(node->left) || split(node->right);
}

void reduce(SnailfishNumber* node) {
    while (explode(node, 0) || split(node));
}

SnailfishNumber* addSnailfishNumbers(SnailfishNumber* a, SnailfishNumber* b) {
    SnailfishNumber* result = new SnailfishNumber(a, b);
    reduce(result);
    return result;
}

int magnitude(SnailfishNumber* node) {
    if (!node) return 0;
    if (node->value != -1) return node->value;
    return 3 * magnitude(node->left) + 2 * magnitude(node->right);
}

void freeSnailfishNumber(SnailfishNumber* node) {
    if (!node) return;
    freeSnailfishNumber(node->left);
    freeSnailfishNumber(node->right);
    delete node;
}

int main() {
    std::ifstream file("input.txt");
    std::string line;
    SnailfishNumber* sum = nullptr;

    while (std::getline(file, line)) {
        size_t index = 0;
        SnailfishNumber* number = parseSnailfishNumber(line, index);
        if (sum) {
            sum = addSnailfishNumbers(sum, number);
        } else {
            sum = number;
        }
    }

    std::cout << "Magnitude of the final sum: " << magnitude(sum) << std::endl;

    freeSnailfishNumber(sum);
    return 0;
}
