
#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <algorithm>
#include <utility>

struct Cube {
    bool on;
    long long x1, x2, y1, y2, z1, z2;
};

long long calculateVolume(const Cube& c) {
    long long vol = (c.x2 - c.x1 + 1) * (c.y2 - c.y1 + 1) * (c.z2 - c.z1 + 1);
    return c.on ? vol : -vol;
}

std::pair<Cube, bool> getIntersection(const Cube& c1, const Cube& c2) {
    long long ix1 = std::max(c1.x1, c2.x1);
    long long ix2 = std::min(c1.x2, c2.x2);
    long long iy1 = std::max(c1.y1, c2.y1);
    long long iy2 = std::min(c1.y2, c2.y2);
    long long iz1 = std::max(c1.z1, c2.z1);
    long long iz2 = std::min(c1.z2, c2.z2);

    if (ix1 > ix2 || iy1 > iy2 || iz1 > iz2) {
        return {Cube{}, false};
    }

    bool intersection_state = c2.on;
    if (c1.on == c2.on) {
        intersection_state = !c1.on;
    }

    return {{intersection_state, ix1, ix2, iy1, iy2, iz1, iz2}, true};
}

std::vector<Cube> parseInput(std::istream& is) {
    std::vector<Cube> cubes;
    std::string line;
    while (std::getline(is, line)) {
        Cube c;
        size_t space_pos = line.find(' ');
        std::string status_str = line.substr(0, space_pos);
        c.on = (status_str == "on");

        size_t x_start = line.find("x=") + 2;
        size_t x_end = line.find("..", x_start);
        c.x1 = std::stoll(line.substr(x_start, x_end - x_start));
        x_start = x_end + 2;
        x_end = line.find(",", x_start);
        c.x2 = std::stoll(line.substr(x_start, x_end - x_start));

        size_t y_start = line.find("y=", x_end) + 2;
        size_t y_end = line.find("..", y_start);
        c.y1 = std::stoll(line.substr(y_start, y_end - y_start));
        y_start = y_end + 2;
        y_end = line.find(",", y_start);
        c.y2 = std::stoll(line.substr(y_start, y_end - y_start));

        size_t z_start = line.find("z=", y_end) + 2;
        size_t z_end = line.find("..", z_start);
        c.z1 = std::stoll(line.substr(z_start, z_end - z_start));
        z_start = z_end + 2;
        c.z2 = std::stoll(line.substr(z_start));

        cubes.push_back(c);
    }
    return cubes;
}

long long solve() {
    std::ifstream inputFile("input.txt");
    std::vector<Cube> cubes = parseInput(inputFile);
    inputFile.close();

    std::vector<Cube> final_list;

    for (const auto& current_cube : cubes) {
        std::vector<Cube> to_add;
        for (const auto& existing_cube : final_list) {
            auto [intersection_cube, did_intersect] = getIntersection(existing_cube, current_cube);
            if (did_intersect) {
                to_add.push_back(intersection_cube);
            }
        }
        if (current_cube.on) {
            to_add.push_back(current_cube);
        }
        final_list.insert(final_list.end(), to_add.begin(), to_add.end());
    }

    long long total_volume = 0;
    for (const auto& c : final_list) {
        total_volume += calculateVolume(c);
    }
    return total_volume;
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);
    long long result = solve();
    std::cout << result << std::endl;
    return 0;
}
