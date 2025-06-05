
#include <algorithm>
#include <fstream>
#include <iostream>
#include <map>
#include <regex>
#include <set>
#include <sstream>
#include <string>
#include <vector>

std::vector<std::string> splitString(const std::string& s, const std::string& delimiter) {
    std::vector<std::string> tokens;
    if (s.empty()) {
        return tokens;
    }

    size_t start = 0;
    size_t end = s.find(delimiter);
    while (end != std::string::npos) {
        std::string token = s.substr(start, end - start);
        if (!token.empty()) {
            tokens.push_back(token);
        }
        start = end + delimiter.length();
        end = s.find(delimiter, start);
    }
    std::string lastToken = s.substr(start);
    if (!lastToken.empty()) {
        tokens.push_back(lastToken);
    }
    return tokens;
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::ifstream inputFile("input.txt");
    if (!inputFile.is_open()) {
        std::cerr << "Error opening input.txt\n";
        return 1;
    }

    std::map<std::string, int> ingredientCount;
    std::vector<std::string> allIngredientsList;
    std::map<std::string, std::set<std::string>> allergenCandidates;

    std::regex linePattern(R"((.*) \(contains (.*)\))");
    std::string line;

    while (std::getline(inputFile, line)) {
        std::smatch matches;
        if (std::regex_match(line, matches, linePattern)) {
            std::string ingredientsStr = matches[1].str();
            std::string allergensStr = matches[2].str();

            std::vector<std::string> currentIngredients = splitString(ingredientsStr, " ");
            std::vector<std::string> currentAllergens = splitString(allergensStr, ", ");

            for (const std::string& ingredient : currentIngredients) {
                ingredientCount[ingredient]++;
                allIngredientsList.push_back(ingredient);
            }

            std::set<std::string> currentIngredientsSet(currentIngredients.begin(), currentIngredients.end());
            for (const std::string& allergen : currentAllergens) {
                if (allergenCandidates.find(allergen) == allergenCandidates.end()) {
                    allergenCandidates[allergen] = currentIngredientsSet;
                } else {
                    std::set<std::string> intersectionSet;
                    std::set<std::string>& existingCandidates = allergenCandidates[allergen];

                    std::set_intersection(existingCandidates.begin(), existingCandidates.end(),
                                          currentIngredientsSet.begin(), currentIngredientsSet.end(),
                                          std::inserter(intersectionSet, intersectionSet.begin()));
                    allergenCandidates[allergen] = intersectionSet;
                }
            }
        }
    }
    inputFile.close();

    std::set<std::string> allUniqueIngredients;
    for (const std::string& ing : allIngredientsList) {
        allUniqueIngredients.insert(ing);
    }

    std::set<std::string> knownAllergenIngredients;
    for (const auto& pair : allergenCandidates) {
        for (const std::string& ing : pair.second) {
            knownAllergenIngredients.insert(ing);
        }
    }

    std::set<std::string> safeIngredients;
    std::set_difference(allUniqueIngredients.begin(), allUniqueIngredients.end(),
                        knownAllergenIngredients.begin(), knownAllergenIngredients.end(),
                        std::inserter(safeIngredients, safeIngredients.begin()));

    long long safeCount = 0;
    for (const std::string& ingredient : safeIngredients) {
        safeCount += ingredientCount[ingredient];
    }
    std::cout << safeCount << std::endl;

    std::map<std::string, std::string> dangerousIngredients;

    while (!allergenCandidates.empty()) {
        bool foundSolved = false;
        for (auto it = allergenCandidates.begin(); it != allergenCandidates.end(); ++it) {
            const std::string& allergen = it->first;
            std::set<std::string>& candidates = it->second;

            if (candidates.size() == 1) {
                std::string ingredient = *candidates.begin();
                dangerousIngredients[allergen] = ingredient;
                
                it = allergenCandidates.erase(it);
                foundSolved = true;

                for (auto& otherPair : allergenCandidates) {
                    otherPair.second.erase(ingredient);
                }
                break;
            }
        }
        if (!foundSolved && !allergenCandidates.empty()) {
            break; 
        }
    }

    std::vector<std::string> sortedAllergens;
    for (const auto& pair : dangerousIngredients) {
        sortedAllergens.push_back(pair.first);
    }
    std::sort(sortedAllergens.begin(), sortedAllergens.end());

    std::stringstream ss;
    for (size_t i = 0; i < sortedAllergens.size(); ++i) {
        ss << dangerousIngredients[sortedAllergens[i]];
        if (i < sortedAllergens.size() - 1) {
            ss << ",";
        }
    }
    std::cout << ss.str() << std::endl;

    return 0;
}
