
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <unordered_map>
#include <unordered_set>
#include <deque>
#include <algorithm>
#include <functional>

using Graph = std::unordered_map<std::string, std::unordered_set<std::string>>;

Graph parse_input(const std::vector<std::string>& input_lines) {
    Graph graph;
    for (const std::string& line : input_lines) {
        size_t colon_pos = line.find(": ");
        std::string vertex = line.substr(0, colon_pos);
        std::string others_str = line.substr(colon_pos + 2);

        graph[vertex]; // Ensure vertex exists in graph

        size_t prev_space = 0;
        size_t space_pos;
        while ((space_pos = others_str.find(' ', prev_space)) != std::string::npos) {
            std::string other_vertex = others_str.substr(prev_space, space_pos - prev_space);
            graph[vertex].insert(other_vertex);
            graph[other_vertex].insert(vertex); // Undirected graph
            prev_space = space_pos + 1;
        }
        std::string other_vertex = others_str.substr(prev_space);
        graph[vertex].insert(other_vertex);
        graph[other_vertex].insert(vertex); // Undirected graph
    }
    return graph;
}

std::pair<bool, std::unordered_map<std::string, std::string>> breadth_first_search(
    const Graph& graph,
    const std::string& start,
    std::function<bool(const std::string&)> goal_func) {

    std::deque<std::string> frontier;
    frontier.push_back(start);

    std::unordered_set<std::string> reached;
    reached.insert(start);

    std::unordered_map<std::string, std::string> came_from;
    came_from[start] = start;

    while (!frontier.empty()) {
        std::string current = frontier.front();
        frontier.pop_front();

        if (goal_func(current)) {
            return {true, came_from};
        }

        if (graph.count(current)) {
            for (const std::string& neighbor : graph.at(current)) {
                if (reached.find(neighbor) == reached.end()) {
                    frontier.push_back(neighbor);
                    reached.insert(neighbor);
                    came_from[neighbor] = current;
                }
            }
        }
    }
    return {false, came_from};
}

std::vector<std::string> reconstruct_path(
    const std::string& start,
    const std::string& end,
    const std::unordered_map<std::string, std::string>& came_from) {

    std::vector<std::string> path;
    std::string current = end;
    while (current != start) {
        path.push_back(current);
        current = came_from.at(current);
    }
    path.push_back(start);
    std::reverse(path.begin(), path.end());
    return path;
}

long long solve(const std::vector<std::string>& input_lines) {
    int min_cut = 3;
    Graph graph = parse_input(input_lines);

    std::string source = graph.begin()->first;
    Graph separate_graph;

    for (const auto& pair : graph) {
        const std::string& end = pair.first;
        if (end == source) {
            continue;
        }

        Graph new_graph = graph; 

        for (int i = 0; i < min_cut; ++i) {
            auto [found_path, came_from] = breadth_first_search(
                new_graph, source, [&end](const std::string& v){ return v == end; });

            if (!found_path) {
                break;
            }

            std::vector<std::string> path = reconstruct_path(source, end, came_from);
            
            for (size_t j = 0; j < path.size() - 1; ++j) {
                const std::string& u = path[j];
                const std::string& v = path[j+1];
                
                new_graph.at(u).erase(v);
                new_graph.at(v).erase(u);
            }
        }
        
        auto [is_valid, _] = breadth_first_search(
            new_graph, source, [&end](const std::string& v){ return v == end; });

        if (!is_valid) {
            separate_graph = new_graph;
            break;
        }
    }

    auto [_, came_from_final] = breadth_first_search(separate_graph, source, [](const std::string&){ return false; });

    long long length1 = came_from_final.size();
    long long length2 = graph.size() - length1;

    return length1 * length2;
}

std::vector<std::string> read_file(const std::string& file_name) {
    std::vector<std::string> lines;
    std::ifstream file(file_name);
    if (!file.is_open()) {
        std::cerr << "Error opening file: " << file_name << std::endl;
        return lines;
    }
    std::string line;
    while (std::getline(file, line)) {
        size_t first = line.find_first_not_of(" \t\n\r");
        if (std::string::npos == first) {
            lines.push_back("");
        } else {
            size_t last = line.find_last_not_of(" \t\n\r");
            lines.push_back(line.substr(first, (last - first + 1)));
        }
    }
    file.close();
    return lines;
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::vector<std::string> input_lines = read_file("input.txt");
    if (input_lines.empty()) {
        return 1;
    }
    std::cout << solve(input_lines) << std::endl;
    return 0;
}
