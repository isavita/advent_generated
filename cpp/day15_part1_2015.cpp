#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <string>
#include <algorithm>
#include <array>

struct Ingredient {
    std::string name;
    int capacity, durability, flavor, texture, calories;
};

std::vector<Ingredient> ingredients;

void parseInput(const std::string& line) {
    Ingredient ingredient;
    std::istringstream iss(line);
    std::string token;
    
    iss >> ingredient.name;
    iss.ignore(100, ' '); // ignore until "capacity"
    iss >> token >> ingredient.capacity;
    iss.ignore(100, ' '); // ignore until "durability"
    iss >> token >> ingredient.durability;
    iss.ignore(100, ' '); // ignore until "flavor"
    iss >> token >> ingredient.flavor;
    iss.ignore(100, ' '); // ignore until "texture"
    iss >> token >> ingredient.texture;
    iss.ignore(100, ' '); // ignore until "calories"
    iss >> token >> ingredient.calories;

    ingredients.push_back(ingredient);
}

int calculateScore(const std::array<int, 100>& amounts) {
    int capacity = 0, durability = 0, flavor = 0, texture = 0;

    for (size_t i = 0; i < ingredients.size(); ++i) {
        capacity += amounts[i] * ingredients[i].capacity;
        durability += amounts[i] * ingredients[i].durability;
        flavor += amounts[i] * ingredients[i].flavor;
        texture += amounts[i] * ingredients[i].texture;
    }

    capacity = std::max(0, capacity);
    durability = std::max(0, durability);
    flavor = std::max(0, flavor);
    texture = std::max(0, texture);

    return capacity * durability * flavor * texture;
}

void findMaxScore(int index, int remaining, std::array<int, 100>& amounts, int& maxScore) {
    if (index == ingredients.size()) {
        if (remaining == 0) {
            maxScore = std::max(maxScore, calculateScore(amounts));
        }
        return;
    }

    for (int i = 0; i <= remaining; ++i) {
        amounts[index] = i;
        findMaxScore(index + 1, remaining - i, amounts, maxScore);
    }
}

int main() {
    std::ifstream file("input.txt");
    std::string line;

    while (std::getline(file, line)) {
        parseInput(line);
    }

    int maxScore = 0;
    std::array<int, 100> amounts = {0};
    findMaxScore(0, 100, amounts, maxScore);

    std::cout << maxScore << std::endl;
    return 0;
}