
#include <iostream>
#include <vector>
#include <string>
#include <queue>
#include <map>
#include <utility> // For std::pair
#include <algorithm> // For std::min
#include <fstream>   // For file input
#include <limits>    // For std::numeric_limits

// Define a Point type for clarity (row, column)
using Point = std::pair<int, int>;

// Define a type for the priority queue elements: (distance, point)
using PQElement = std::pair<int, Point>;

// Custom comparator for min-priority queue (smallest distance at top)
struct ComparePQElement {
    bool operator()(const PQElement& a, const PQElement& b) {
        return a.first > b.first; // Min-heap based on distance (a is "greater" if its distance is larger)
    }
};

// Dijkstra's algorithm to find shortest paths from a source 'end'
// on a grid with height constraints (reversed graph traversal logic)
std::map<Point, int> djikstra(const std::vector<std::string>& grid, Point end) {
    std::priority_queue<PQElement, std::vector<PQElement>, ComparePQElement> pq;
    std::map<Point, int> dist;

    pq.push({0, end});
    dist[end] = 0;

    int rows = grid.size();
    int cols = grid[0].length();

    int dr[] = {-1, 1, 0, 0}; // Row changes for neighbors (Up, Down)
    int dc[] = {0, 0, -1, 1}; // Column changes for neighbors (Left, Right)

    while (!pq.empty()) {
        int curr_dist = pq.top().first;
        Point curr = pq.top().second;
        pq.pop();

        if (curr_dist > dist[curr]) {
            continue; // Skip if a shorter path to 'curr' has already been found
        }

        for (int i = 0; i < 4; ++i) {
            Point next_p = {curr.first + dr[i], curr.second + dc[i]};

            if (next_p.first < 0 || next_p.first >= rows || next_p.second < 0 || next_p.second >= cols) {
                continue; // Skip if neighbor is out of bounds
            }

            // Height constraint for reverse traversal:
            // A valid forward step from 'next_p' to 'curr' implies h(curr) <= h(next_p) + 1.
            // If `h(curr) > h(next_p) + 1`, then it's an invalid reverse step.
            if (static_cast<int>(grid[curr.first][curr.second]) - static_cast<int>(grid[next_p.first][next_p.second]) > 1) {
                continue;
            }

            int next_dist = curr_dist + 1;

            if (dist.find(next_p) == dist.end() || next_dist < dist[next_p]) {
                dist[next_p] = next_dist;
                pq.push({next_dist, next_p});
            }
        }
    }
    return dist;
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::vector<std::string> grid_data;
    Point start_pos, end_pos;
    std::vector<Point> a_positions;

    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cerr << "Error opening input.txt" << std::endl;
        return 1;
    }

    std::string line;
    int y = 0; // Row index
    while (std::getline(file, line)) {
        grid_data.push_back(line);
        for (int x = 0; x < line.length(); ++x) { // Column index
            char b = line[x];
            Point p = {y, x};

            if (b == 'S') {
                start_pos = p;
            } else if (b == 'E') {
                end_pos = p;
            } else if (b == 'a') {
                a_positions.push_back(p);
            }
        }
        y++;
    }
    file.close();

    grid_data[start_pos.first][start_pos.second] = 'a';
    grid_data[end_pos.first][end_pos.second] = 'z';

    std::map<Point, int> dists = djikstra(grid_data, end_pos);

    int shortest_path_len = std::numeric_limits<int>::max();

    if (dists.count(start_pos)) {
        shortest_path_len = dists[start_pos];
    }

    for (const auto& a_pos : a_positions) {
        if (dists.count(a_pos)) {
            shortest_path_len = std::min(shortest_path_len, dists[a_pos]);
        }
    }

    std::cout << shortest_path_len << std::endl;

    return 0;
}
