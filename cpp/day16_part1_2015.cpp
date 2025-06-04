
#include <iostream>
#include <fstream>
#include <string>
#include <map>
#include <sstream>

int main() {
    std::map<std::string, int> mfcsam_data;
    mfcsam_data["children"] = 3;
    mfcsam_data["cats"] = 7;
    mfcsam_data["samoyeds"] = 2;
    mfcsam_data["pomeranians"] = 3;
    mfcsam_data["akitas"] = 0;
    mfcsam_data["vizslas"] = 0;
    mfcsam_data["goldfish"] = 5;
    mfcsam_data["trees"] = 3;
    mfcsam_data["cars"] = 2;
    mfcsam_data["perfumes"] = 1;

    std::ifstream inputFile("input.txt");
    if (!inputFile.is_open()) {
        return 1;
    }

    std::string line;
    while (std::getline(inputFile, line)) {
        size_t sue_num_start = line.find("Sue ") + 4;
        size_t sue_num_end = line.find(":", sue_num_start);
        int sue_num = std::stoi(line.substr(sue_num_start, sue_num_end - sue_num_start));

        std::string attributes_str = line.substr(sue_num_end + 2); // Skip ": "

        std::stringstream ss(attributes_str);
        std::string segment;
        bool current_sue_matches = true;

        while (std::getline(ss, segment, ',')) {
            size_t colon_pos = segment.find(":");
            
            std::string attr_name = segment.substr(0, colon_pos);
            if (!attr_name.empty() && attr_name[0] == ' ') {
                attr_name = attr_name.substr(1); // Trim leading space
            }

            int attr_val = std::stoi(segment.substr(colon_pos + 2)); // Skip ": "

            if (mfcsam_data.at(attr_name) != attr_val) {
                current_sue_matches = false;
                break;
            }
        }

        if (current_sue_matches) {
            std::cout << sue_num << std::endl;
            break;
        }
    }

    inputFile.close();
    return 0;
}

