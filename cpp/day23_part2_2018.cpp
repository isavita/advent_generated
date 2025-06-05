
#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <algorithm>
#include <queue>
#include <limits>
#include <cstdio>

struct Nanobot {
    int x, y, z, r;
};

long long manhattanDistance(int ax, int ay, int az, int bx, int by, int bz) {
    return std::abs(static_cast<long long>(ax) - bx) +
           std::abs(static_cast<long long>(ay) - by) +
           std::abs(static_cast<long long>(az) - bz);
}

long long minDistanceToOrigin(int x, int y, int z, int size) {
    long long dx = 0;
    if (x > 0) {
        dx = x;
    } else if (static_cast<long long>(x) + size - 1 < 0) {
        dx = -(static_cast<long long>(x) + size - 1);
    }

    long long dy = 0;
    if (y > 0) {
        dy = y;
    } else if (static_cast<long long>(y) + size - 1 < 0) {
        dy = -(static_cast<long long>(y) + size - 1);
    }

    long long dz = 0;
    if (z > 0) {
        dz = z;
    } else if (static_cast<long long>(z) + size - 1 < 0) {
        dz = -(static_cast<long long>(z) + size - 1);
    }

    return dx + dy + dz;
}

std::vector<Nanobot> parseInput(const std::string& filePath) {
    std::vector<Nanobot> nanobots;
    std::ifstream file(filePath);
    std::string line;

    if (!file.is_open()) {
        return nanobots;
    }

    while (std::getline(file, line)) {
        Nanobot nb;
        if (sscanf(line.c_str(), "pos=<%d,%d,%d>, r=%d",
                   &nb.x, &nb.y, &nb.z, &nb.r) == 4) {
            nanobots.push_back(nb);
        }
    }
    return nanobots;
}

int partOne(const std::vector<Nanobot>& nanobots) {
    if (nanobots.empty()) {
        return 0;
    }

    const Nanobot* strongest = &nanobots[0];
    for (size_t i = 1; i < nanobots.size(); ++i) {
        if (nanobots[i].r > strongest->r) {
            strongest = &nanobots[i];
        }
    }

    int count = 0;
    for (const auto& bot : nanobots) {
        if (manhattanDistance(strongest->x, strongest->y, strongest->z, bot.x, bot.y, bot.z) <= strongest->r) {
            count++;
        }
    }
    return count;
}

struct Cube {
    int count;
    long long dist_to_origin;
    int size;
    int x, y, z;
};

struct CompareCube {
    bool operator()(const Cube& a, const Cube& b) {
        if (a.count != b.count) {
            return a.count < b.count;
        }
        if (a.dist_to_origin != b.dist_to_origin) {
            return a.dist_to_origin > b.dist_to_origin;
        }
        return a.size > b.size;
    }
};

long long partTwo(const std::vector<Nanobot>& nanobots) {
    if (nanobots.empty()) {
        return 0;
    }

    int min_x = std::numeric_limits<int>::max();
    int max_x = std::numeric_limits<int>::min();
    int min_y = std::numeric_limits<int>::max();
    int max_y = std::numeric_limits<int>::min();
    int min_z = std::numeric_limits<int>::max();
    int max_z = std::numeric_limits<int>::min();

    for (const auto& bot : nanobots) {
        min_x = std::min(min_x, bot.x);
        max_x = std::max(max_x, bot.x);
        min_y = std::min(min_y, bot.y);
        max_y = std::max(max_y, bot.y);
        min_z = std::min(min_z, bot.z);
        max_z = std::max(max_z, bot.z);
    }

    int max_dim_len = 1;
    if (!nanobots.empty()) {
        max_dim_len = std::max({max_x - min_x + 1, max_y - min_y + 1, max_z - min_z + 1});
    }
    
    int size = 1;
    while (size < max_dim_len) {
        size *= 2;
    }

    std::priority_queue<Cube, std::vector<Cube>, CompareCube> pq;
    
    int initial_count = 0;
    for(const auto& bot : nanobots) {
        long long d = 0;
        if (bot.x < min_x) d += min_x - bot.x;
        else if (bot.x > static_cast<long long>(min_x) + size - 1) d += bot.x - (static_cast<long long>(min_x) + size - 1);

        if (bot.y < min_y) d += min_y - bot.y;
        else if (bot.y > static_cast<long long>(min_y) + size - 1) d += bot.y - (static_cast<long long>(min_y) + size - 1);

        if (bot.z < min_z) d += min_z - bot.z;
        else if (bot.z > static_cast<long long>(min_z) + size - 1) d += bot.z - (static_cast<long long>(min_z) + size - 1);
        
        if (d <= bot.r) {
            initial_count++;
        }
    }

    pq.push({initial_count, minDistanceToOrigin(min_x, min_y, min_z, size), size, min_x, min_y, min_z});

    long long best_distance = std::numeric_limits<long long>::max();
    int best_count = -1;

    while (!pq.empty()) {
        Cube current = pq.top();
        pq.pop();

        if (current.count < best_count) {
            break; 
        }

        if (current.size == 1) {
            best_count = current.count;
            best_distance = current.dist_to_origin;
            break;
        }

        int half = current.size / 2;
        for (int dx_idx = 0; dx_idx < 2; ++dx_idx) {
            for (int dy_idx = 0; dy_idx < 2; ++dy_idx) {
                for (int dz_idx = 0; dz_idx < 2; ++dz_idx) {
                    int nx = current.x + dx_idx * half;
                    int ny = current.y + dy_idx * half;
                    int nz = current.z + dz_idx * half;
                    int new_size = half;

                    int count = 0;
                    for (const auto& bot : nanobots) {
                        long long d = 0;
                        if (bot.x < nx) d += nx - bot.x;
                        else if (bot.x > static_cast<long long>(nx) + new_size - 1) d += bot.x - (static_cast<long long>(nx) + new_size - 1);

                        if (bot.y < ny) d += ny - bot.y;
                        else if (bot.y > static_cast<long long>(ny) + new_size - 1) d += bot.y - (static_cast<long long>(ny) + new_size - 1);

                        if (bot.z < nz) d += nz - bot.z;
                        else if (bot.z > static_cast<long long>(nz) + new_size - 1) d += bot.z - (static_cast<long long>(nz) + new_size - 1);
                        
                        if (d <= bot.r) {
                            count++;
                        }
                    }

                    long long distance = minDistanceToOrigin(nx, ny, nz, new_size);

                    if (count < best_count) {
                        continue;
                    }
                    
                    pq.push({count, distance, new_size, nx, ny, nz});
                }
            }
        }
    }
    return best_distance;
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::vector<Nanobot> nanobots = parseInput("input.txt");

    int count_in_range = partOne(nanobots);
    std::cout << count_in_range << std::endl;

    long long shortest_distance = partTwo(nanobots);
    std::cout << shortest_distance << std::endl;

    return 0;
}
