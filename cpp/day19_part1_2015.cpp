
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <unordered_map>

int main() {
    std::ifstream file("input.txt");
    std::string line;
    std::vector<std::string> replacements;
    std::string molecule;

    while (std::getline(file, line)) {
        if (line.empty()) {
            continue;
        }
        if (line.find(" => ") != std::string::npos) {
            replacements.push_back(line);
        } else {
            molecule = line;
        }
    }

    std::unordered_map<std::string, bool> molecules;
    for (const auto& replacement : replacements) {
        size_t pos = 0;
        while ((pos = molecule.find(replacement.substr(0, replacement.find(" => ")), pos)) != std::string::npos) {
            std::string newMolecule = molecule;
            newMolecule.replace(pos, replacement.substr(0, replacement.find(" => ")).length(), replacement.substr(replacement.find(" => ") + 4));
            molecules[newMolecule] = true;
            pos += replacement.substr(0, replacement.find(" => ")).length();
        }
    }

    std::cout << molecules.size() << std::endl;

    return 0;
}
