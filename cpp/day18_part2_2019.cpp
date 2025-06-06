
#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <map>
#include <queue>
#include <cstdint>
#include <utility>

using namespace std;

struct Point {
    int x, y;

    bool operator<(const Point& other) const {
        if (y != other.y) return y < other.y;
        return x < other.x;
    }
};

struct State {
    vector<Point> p;
    uint32_t keys_mask;
    int worker_id;

    bool operator<(const State& other) const {
        if (keys_mask != other.keys_mask) return keys_mask < other.keys_mask;
        if (worker_id != other.worker_id) return worker_id < other.worker_id;
        return p < other.p;
    }
};

int main() {
    ios_base::sync_with_stdio(false);
    cin.tie(NULL);

    ifstream file("input.txt");
    string line;
    map<Point, char> grid;
    Point start_pos;
    uint32_t all_keys_mask = 0;
    int y = 0;
    while (getline(file, line)) {
        for (int x = 0; x < (int)line.length(); ++x) {
            char c = line[x];
            if (c != '.') grid[{x, y}] = c;
            if (c >= 'a' && c <= 'z') all_keys_mask |= (1 << (c - 'a'));
            else if (c == '@') start_pos = {x, y};
        }
        y++;
    }

    grid[start_pos] = '#';
    const int dx[] = {0, 0, 1, -1};
    const int dy[] = {1, -1, 0, 0};
    for (int i = 0; i < 4; ++i) {
        grid[{start_pos.x + dx[i], start_pos.y + dy[i]}] = '#';
    }

    vector<Point> starts;
    const int diag_dx[] = {-1, -1, 1, 1};
    const int diag_dy[] = {-1, 1, -1, 1};
    for (int i = 0; i < 4; ++i) {
        Point new_start = {start_pos.x + diag_dx[i], start_pos.y + diag_dy[i]};
        grid[new_start] = '@';
        starts.push_back(new_start);
    }

    using PQElement = pair<int, State>;
    priority_queue<PQElement, vector<PQElement>, greater<PQElement>> pq;
    map<State, int> dist;

    for (int i = 0; i < 4; ++i) {
        State initial_state = {starts, all_keys_mask, i};
        dist[initial_state] = 0;
        pq.push({0, initial_state});
    }

    while (!pq.empty()) {
        auto [d, current_state] = pq.top();
        pq.pop();

        if (d > dist.at(current_state)) continue;
        if (current_state.keys_mask == 0) {
            cout << d << endl;
            return 0;
        }
        
        int worker_id = current_state.worker_id;
        Point current_pos = current_state.p[worker_id];

        for (int i = 0; i < 4; ++i) {
            Point next_pos = {current_pos.x + dx[i], current_pos.y + dy[i]};
            
            auto it = grid.find(next_pos);
            char cell = (it == grid.end()) ? '.' : it->second;

            if (cell == '#') continue;
            if (cell >= 'A' && cell <= 'Z') {
                if ((current_state.keys_mask & (1 << (cell - 'A'))) != 0) continue;
            }

            State next_state = current_state;
            next_state.p[worker_id] = next_pos;
            int next_dist = d + 1;
            bool found_new_key = false;

            if (cell >= 'a' && cell <= 'z') {
                uint32_t key_bit = 1 << (cell - 'a');
                if ((next_state.keys_mask & key_bit) != 0) {
                    found_new_key = true;
                    next_state.keys_mask &= ~key_bit;
                }
            }
            
            if (found_new_key) {
                for (int j = 0; j < 4; ++j) {
                    State s = next_state;
                    s.worker_id = j;
                    if (!dist.count(s) || next_dist < dist.at(s)) {
                        dist[s] = next_dist;
                        pq.push({next_dist, s});
                    }
                }
            } else {
                if (!dist.count(next_state) || next_dist < dist.at(next_state)) {
                    dist[next_state] = next_dist;
                    pq.push({next_dist, next_state});
                }
            }
        }
    }

    return 0;
}
