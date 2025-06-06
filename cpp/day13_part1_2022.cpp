
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <cctype>

struct Item {
    bool is_list = false;
    int value = 0;
    std::vector<Item> list;
};

Item parse(const char*& s) {
    Item item;
    if (*s == '[') {
        item.is_list = true;
        s++; // Skip '['
        while (*s != ']') {
            item.list.push_back(parse(s));
            if (*s == ',') {
                s++;
            }
        }
        s++; // Skip ']'
    } else {
        int val = 0;
        while (std::isdigit(*s)) {
            val = val * 10 + (*s - '0');
            s++;
        }
        item.value = val;
    }
    return item;
}

int compare(const Item& left, const Item& right) {
    if (!left.is_list && !right.is_list) {
        if (left.value < right.value) return -1;
        if (left.value > right.value) return 1;
        return 0;
    }

    if (left.is_list && right.is_list) {
        for (size_t i = 0; i < left.list.size() && i < right.list.size(); ++i) {
            int result = compare(left.list[i], right.list[i]);
            if (result != 0) return result;
        }
        if (left.list.size() < right.list.size()) return -1;
        if (left.list.size() > right.list.size()) return 1;
        return 0;
    }

    if (left.is_list) {
        Item right_as_list;
        right_as_list.is_list = true;
        right_as_list.list.push_back(right);
        return compare(left, right_as_list);
    }
    
    Item left_as_list;
    left_as_list.is_list = true;
    left_as_list.list.push_back(left);
    return compare(left_as_list, right);
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::ifstream file("input.txt");
    std::string line1, line2, blank;
    int sum = 0;
    int index = 1;

    while (std::getline(file, line1) && std::getline(file, line2)) {
        const char* p1 = line1.c_str();
        const char* p2 = line2.c_str();
        Item left = parse(p1);
        Item right = parse(p2);

        if (compare(left, right) < 0) {
            sum += index;
        }
        index++;
        std::getline(file, blank);
    }

    std::cout << sum << std::endl;

    return 0;
}
