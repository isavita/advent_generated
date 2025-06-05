
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <map>
#include <algorithm>
#include <sstream>

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::ifstream file("input.txt");
    if (!file.is_open()) {
        return 1;
    }

    std::map<std::string, int> guest_to_id;
    std::vector<std::vector<int>> happiness_matrix;
    int next_id = 0;

    auto get_guest_id = [&](const std::string& name) {
        if (guest_to_id.find(name) == guest_to_id.end()) {
            guest_to_id[name] = next_id;
            
            for (auto& row : happiness_matrix) {
                row.resize(next_id + 1, 0);
            }
            happiness_matrix.emplace_back(next_id + 1, 0);
            
            next_id++;
        }
        return guest_to_id[name];
    };

    std::string line;
    while (std::getline(file, line)) {
        std::stringstream ss(line);
        std::string p1_name, would_str, gain_lose_str, units_str, happiness_str, by_str, sitting_str, next_str, to_str, p2_name_dotted;
        int change_val;

        ss >> p1_name >> would_str >> gain_lose_str >> change_val >> happiness_str >> units_str >> by_str >> sitting_str >> next_str >> to_str >> p2_name_dotted;

        std::string p2_name = p2_name_dotted.substr(0, p2_name_dotted.length() - 1);

        int p1_id = get_guest_id(p1_name);
        int p2_id = get_guest_id(p2_name);

        if (gain_lose_str == "lose") {
            change_val = -change_val;
        }
        
        happiness_matrix[p1_id][p2_id] = change_val;
    }
    file.close();

    get_guest_id("You"); 

    int total_guests = next_id;

    std::vector<int> arrangement_ids(total_guests);
    for (int i = 0; i < total_guests; ++i) {
        arrangement_ids[i] = i;
    }

    long long max_total_happiness = 0;

    std::sort(arrangement_ids.begin(), arrangement_ids.end());

    do {
        long long current_total_happiness = 0;
        for (int i = 0; i < total_guests; ++i) {
            int person1_id = arrangement_ids[i];
            int person2_id = arrangement_ids[(i + 1) % total_guests];

            current_total_happiness += happiness_matrix[person1_id][person2_id];
            current_total_happiness += happiness_matrix[person2_id][person1_id];
        }
        max_total_happiness = std::max(max_total_happiness, current_total_happiness);
    } while (std::next_permutation(arrangement_ids.begin(), arrangement_ids.end()));

    std::cout << max_total_happiness << std::endl;

    return 0;
}
