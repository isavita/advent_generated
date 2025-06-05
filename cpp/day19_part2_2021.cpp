
#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <sstream>
#include <numeric>
#include <algorithm>
#include <cmath>
#include <set>
#include <map>
#include <unordered_set>
#include <unordered_map>
#include <functional> // For std::hash

struct Point {
    int x, y, z;

    bool operator==(const Point& other) const {
        return x == other.x && y == other.y && z == other.z;
    }

    // Needed for std::set and std::map keys (for std::set/map based containers)
    bool operator<(const Point& other) const {
        if (x != other.x) return x < other.x;
        if (y != other.y) return y < other.y;
        return z < other.z;
    }
};

// Custom hash function for Point to use with unordered_set/map
namespace std {
    template <>
    struct hash<Point> {
        size_t operator()(const Point& p) const {
            // A simple way to combine hashes for good distribution
            // Boost's hash_combine is often used, but this is a common approximation.
            size_t h1 = hash<int>()(p.x);
            size_t h2 = hash<int>()(p.y);
            size_t h3 = hash<int>()(p.z);
            size_t seed = 0;
            seed ^= h1 + 0x9e3779b9 + (seed << 6) + (seed >> 2);
            seed ^= h2 + 0x9e3779b9 + (seed << 6) + (seed >> 2);
            seed ^= h3 + 0x9e3779b9 + (seed << 6) + (seed >> 2);
            return seed;
        }
    };
}

Point operator+(const Point& p1, const Point& p2) {
    return {p1.x + p2.x, p1.y + p2.y, p1.z + p2.z};
}

Point operator-(const Point& p1, const Point& p2) {
    return {p1.x - p2.x, p1.y - p2.y, p1.z - p2.z};
}

struct Matrix3x3 {
    int m[3][3];

    Point transform(const Point& p) const {
        return {
            p.x * m[0][0] + p.y * m[0][1] + p.z * m[0][2],
            p.x * m[1][0] + p.y * m[1][1] + p.z * m[1][2],
            p.x * m[2][0] + p.y * m[2][1] + p.z * m[2][2]
        };
    }
};

std::vector<Matrix3x3> get_rotations() {
    std::vector<Matrix3x3> rotations;
    int p[] = {0, 1, 2}; // Permutation of axes
    do {
        for (int i = 0; i < (1 << 3); ++i) { // 2^3 combinations for signs
            Matrix3x3 mat = {{{0,0,0},{0,0,0},{0,0,0}}};
            int signs[3];
            for (int j = 0; j < 3; ++j) {
                signs[j] = (i & (1 << j)) ? 1 : -1;
            }

            for (int row = 0; row < 3; ++row) {
                mat.m[row][p[row]] = signs[row];
            }

            // Calculate determinant to ensure proper rotation (no reflection)
            int det = (
                mat.m[0][0]*(mat.m[1][1]*mat.m[2][2] - mat.m[1][2]*mat.m[2][1]) -
                mat.m[0][1]*(mat.m[1][0]*mat.m[2][2] - mat.m[1][2]*mat.m[2][0]) +
                mat.m[0][2]*(mat.m[1][0]*mat.m[2][1] - mat.m[1][1]*mat.m[2][0])
            );

            if (det == 1) {
                rotations.push_back(mat);
            }
        }
    } while (std::next_permutation(p, p + 3)); // Permute axes
    return rotations;
}

std::vector<std::vector<Point>> read_input(const std::string& filename) {
    std::vector<std::vector<Point>> scanners;
    std::ifstream file(filename);
    std::string line;
    std::vector<Point> current_scanner;

    while (std::getline(file, line)) {
        if (line.rfind("---", 0) == 0) { // Check if line starts with "---"
            if (!current_scanner.empty()) {
                scanners.push_back(current_scanner);
                current_scanner.clear();
            }
        } else if (!line.empty()) {
            size_t p1 = line.find(',');
            size_t p2 = line.find(',', p1 + 1);
            int x = std::stoi(line.substr(0, p1));
            int y = std::stoi(line.substr(p1 + 1, p2 - (p1 + 1)));
            int z = std::stoi(line.substr(p2 + 1));
            current_scanner.push_back({x, y, z});
        }
    }
    if (!current_scanner.empty()) {
        scanners.push_back(current_scanner);
    }
    return scanners;
}

int manhattan(const Point& p1, const Point& p2) {
    return std::abs(p1.x - p2.x) + std::abs(p1.y - p2.y) + std::abs(p1.z - p2.z);
}

int solve(const std::vector<std::vector<Point>>& all_scanners) {
    std::vector<Matrix3x3> rotations = get_rotations();

    std::unordered_set<int> aligned_scanners;
    std::map<int, Point> scanner_positions; // Use map for scanner_positions as keys are just int indices
    std::unordered_set<Point> known_beacons;

    aligned_scanners.insert(0);
    scanner_positions[0] = {0, 0, 0};
    for (const auto& p : all_scanners[0]) {
        known_beacons.insert(p);
    }

    std::unordered_set<int> pending_scanners;
    for (int i = 1; i < all_scanners.size(); ++i) {
        pending_scanners.insert(i);
    }

    while (!pending_scanners.empty()) {
        bool found_alignment_in_iteration = false;
        std::vector<int> current_pending_list(pending_scanners.begin(), pending_scanners.end());

        for (int scanner_idx : current_pending_list) {
            if (aligned_scanners.count(scanner_idx)) continue;

            const auto& current_scanner_beacons = all_scanners[scanner_idx];
            bool aligned_current_scanner = false;

            for (const auto& rot_matrix : rotations) {
                std::vector<Point> rotated_beacons;
                rotated_beacons.reserve(current_scanner_beacons.size());
                for (const auto& p : current_scanner_beacons) {
                    rotated_beacons.push_back(rot_matrix.transform(p));
                }

                std::unordered_map<Point, int> deltas;
                for (const auto& rotated_beacon : rotated_beacons) {
                    for (const auto& known_beacon : known_beacons) {
                        Point delta = known_beacon - rotated_beacon;
                        deltas[delta]++;
                    }
                }

                Point best_delta = {0, 0, 0};
                int max_count = 0;
                for (const auto& pair : deltas) {
                    if (pair.second > max_count) {
                        max_count = pair.second;
                        best_delta = pair.first;
                    }
                }

                if (max_count >= 12) {
                    scanner_positions[scanner_idx] = best_delta;
                    for (const auto& p : rotated_beacons) {
                        known_beacons.insert(p + best_delta);
                    }
                    aligned_scanners.insert(scanner_idx);
                    pending_scanners.erase(scanner_idx);
                    aligned_current_scanner = true;
                    found_alignment_in_iteration = true;
                    break;
                }
            }
            if (aligned_current_scanner) {
                break; // Restart the outer loop to process newly aligned scanners first
            }
        }
        if (!found_alignment_in_iteration && !pending_scanners.empty()) {
            // No new scanners could be aligned in this pass. Break to prevent infinite loop.
            break;
        }
    }

    int max_dist = 0;
    std::vector<Point> positions;
    positions.reserve(scanner_positions.size());
    for (const auto& pair : scanner_positions) {
        positions.push_back(pair.second);
    }

    for (size_t i = 0; i < positions.size(); ++i) {
        for (size_t j = i + 1; j < positions.size(); ++j) {
            max_dist = std::max(max_dist, manhattan(positions[i], positions[j]));
        }
    }
    return max_dist;
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::vector<std::vector<Point>> scanners = read_input("input.txt");
    int result = solve(scanners);
    std::cout << result << std::endl;

    return 0;
}
