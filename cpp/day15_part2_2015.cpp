
#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <algorithm>

using namespace std;

struct Ingredient {
    string name;
    int capacity;
    int durability;
    int flavor;
    int texture;
    int calories;
};

vector<Ingredient> read_ingredients(const string& filename) {
    vector<Ingredient> ingredients;
    ifstream file(filename);
    string line;

    while (getline(file, line)) {
        stringstream ss(line);
        string word;
        vector<string> parts;
        while (ss >> word) {
            parts.push_back(word);
        }

        if (parts.size() < 11) continue;

        Ingredient ingredient;
        ingredient.name = parts[0];
        ingredient.capacity = stoi(parts[2].substr(0, parts[2].size() - 1));
        ingredient.durability = stoi(parts[4].substr(0, parts[4].size() - 1));
        ingredient.flavor = stoi(parts[6].substr(0, parts[6].size() - 1));
        ingredient.texture = stoi(parts[8].substr(0, parts[8].size() - 1));
        ingredient.calories = stoi(parts[10]);
        ingredients.push_back(ingredient);
    }
    return ingredients;
}

int score(const vector<Ingredient>& ingredients, const vector<int>& teaspoons) {
    int capacity = 0, durability = 0, flavor = 0, texture = 0;
    for (size_t i = 0; i < ingredients.size(); ++i) {
        capacity += ingredients[i].capacity * teaspoons[i];
        durability += ingredients[i].durability * teaspoons[i];
        flavor += ingredients[i].flavor * teaspoons[i];
        texture += ingredients[i].texture * teaspoons[i];
    }

    capacity = max(0, capacity);
    durability = max(0, durability);
    flavor = max(0, flavor);
    texture = max(0, texture);

    return capacity * durability * flavor * texture;
}

int calculate_calories(const vector<Ingredient>& ingredients, const vector<int>& teaspoons) {
    int calories = 0;
    for (size_t i = 0; i < ingredients.size(); ++i) {
        calories += ingredients[i].calories * teaspoons[i];
    }
    return calories;
}

int calculate_max_score(const vector<Ingredient>& ingredients, int index, int remaining, vector<int>& teaspoons, int target_calories) {
    if (index == ingredients.size() - 1) {
        teaspoons[index] = remaining;
        if (calculate_calories(ingredients, teaspoons) == target_calories) {
            return score(ingredients, teaspoons);
        }
        return 0;
    }

    int max_score = 0;
    for (int i = 0; i <= remaining; ++i) {
        teaspoons[index] = i;
        int current_score = calculate_max_score(ingredients, index + 1, remaining - i, teaspoons, target_calories);
        max_score = max(max_score, current_score);
    }
    return max_score;
}

int find_max_score(const vector<Ingredient>& ingredients, int total_teaspoons, int target_calories) {
    vector<int> teaspoons(ingredients.size());
    return calculate_max_score(ingredients, 0, total_teaspoons, teaspoons, target_calories);
}

int main() {
    vector<Ingredient> ingredients = read_ingredients("input.txt");
    int max_score = find_max_score(ingredients, 100, 500);
    cout << max_score << endl;
    return 0;
}
