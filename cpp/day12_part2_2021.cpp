
#include <cctype>
#include <fstream>
#include <iostream>
#include <string>
#include <unordered_map>
#include <vector>

std::unordered_map<std::string, int> node_to_id;
std::vector<std::string> id_to_node;
std::vector<bool> is_small_cave_vec;
std::vector<std::vector<int>> adj;
int START_NODE_ID = -1;
int END_NODE_ID = -1;

int get_or_create_node_id(const std::string& name) {
    if (node_to_id.find(name) == node_to_id.end()) {
        int id = id_to_node.size();
        node_to_id[name] = id;
        id_to_node.push_back(name);

        bool small = true;
        for (char c : name) {
            if (!std::islower(c)) {
                small = false;
                break;
            }
        }
        is_small_cave_vec.push_back(small);
        adj.emplace_back();
        return id;
    }
    return node_to_id[name];
}

long long find_paths(int u, int end_node_id, std::vector<bool> visited_state,
                     bool small_twice_allowed_for_path) {
    if (u == end_node_id) {
        return 1;
    }

    if (is_small_cave_vec[u]) {
        if (visited_state[u]) {
            if (small_twice_allowed_for_path) {
                small_twice_allowed_for_path = false;
            } else {
                return 0;
            }
        }
        visited_state[u] = true;
    }

    long long paths = 0;
    for (int v : adj[u]) {
        paths +=
            find_paths(v, end_node_id, visited_state, small_twice_allowed_for_path);
    }
    return paths;
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::ifstream infile("input.txt");
    std::string line;

    while (std::getline(infile, line)) {
        size_t dash_pos = line.find('-');
        std::string s1 = line.substr(0, dash_pos);
        std::string s2 = line.substr(dash_pos + 1);

        int id1 = get_or_create_node_id(s1);
        int id2 = get_or_create_node_id(s2);

        if (s1 == "start") START_NODE_ID = id1;
        if (s2 == "start") START_NODE_ID = id2;
        if (s1 == "end") END_NODE_ID = id1;
        if (s2 == "end") END_NODE_ID = id2;

        if (s1 != "end" && s2 != "start") {
            adj[id1].push_back(id2);
        }
        if (s2 != "end" && s1 != "start") {
            adj[id2].push_back(id1);
        }
    }
    infile.close();

    std::vector<bool> initial_visited(id_to_node.size(), false);

    std::cout << find_paths(START_NODE_ID, END_NODE_ID, initial_visited, false)
              << std::endl;
    std::cout << find_paths(START_NODE_ID, END_NODE_ID, initial_visited, true)
              << std::endl;

    return 0;
}
