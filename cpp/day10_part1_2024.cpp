
#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <set>
#include <stack>
#include <utility>

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::ifstream inputFile("input.txt");
    std::vector<std::string> lines;
    std::string line;
    while (std::getline(inputFile, line)) {
        lines.push_back(line);
    }
    inputFile.close();

    int nr = lines.size();
    if (nr == 0) {
        std::cout << 0 << std::endl;
        return 0;
    }
    int nc = lines[0].length();

    std::vector<std::vector<int>> grid(nr, std::vector<int>(nc));
    for (int r = 0; r < nr; ++r) {
        for (int c = 0; c < nc; ++c) {
            grid[r][c] = lines[r][c] - '0';
        }
    }

    std::vector<std::pair<int, int>> trailheads;
    for (int r = 0; r < nr; ++r) {
        for (int c = 0; c < nc; ++c) {
            if (grid[r][c] == 0) {
                trailheads.push_back({r, c});
            }
        }
    }

    int dr[] = {1, -1, 0, 0};
    int dc[] = {0, 0, 1, -1};

    long long sum_scores = 0;

    for (const auto& th : trailheads) {
        std::set<std::pair<int, int>> reached;
        std::stack<std::pair<std::pair<int, int>, int>> front;
        std::vector<std::vector<std::vector<bool>>> visited_states(nr, std::vector<std::vector<bool>>(nc, std::vector<bool>(10, false)));

        front.push({th, 0});
        visited_states[th.first][th.second][0] = true;

        while (!front.empty()) {
            auto current_state = front.top();
            front.pop();

            int r = current_state.first.first;
            int c = current_state.first.second;
            int h = current_state.second;

            if (h == 9) {
                reached.insert({r, c});
                continue;
            }

            for (int i = 0; i < 4; ++i) {
                int nr2 = r + dr[i];
                int nc2 = c + dc[i];

                if (nr2 >= 0 && nr2 < nr && nc2 >= 0 && nc2 < nc) {
                    if (grid[nr2][nc2] == h + 1) {
                        if (!visited_states[nr2][nc2][h + 1]) {
                            visited_states[nr2][nc2][h + 1] = true;
                            front.push({{nr2, nc2}, h + 1});
                        }
                    }
                }
            }
        }
        sum_scores += reached.size();
    }

    std::cout << sum_scores << std::endl;

    return 0;
}
