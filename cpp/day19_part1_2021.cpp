
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <set>
#include <sstream>
#include <algorithm>
#include <functional> // For std::function

struct Point {
    int x, y, z;

    bool operator<(const Point& other) const {
        if (x != other.x) return x < other.x;
        if (y != other.y) return y < other.y;
        return z < other.z;
    }

    bool operator==(const Point& other) const {
        return x == other.x && y == other.y && z == other.z;
    }
};

struct Scanner {
    int number;
    int x, y, z;
    std::vector<Point> relative_coords;
    std::vector<Point> absolute_coords;
    std::set<Point> absolute_coords_map;
    std::vector<std::vector<Point>> rotations;

    Scanner(int num, const std::vector<Point>& coords) :
        number(num), x(0), y(0), z(0), relative_coords(coords) {
        fill_rotations();
    }

    void fill_absolute_coords_map() {
        absolute_coords_map.clear();
        for (const auto& p : absolute_coords) {
            absolute_coords_map.insert(p);
        }
    }

private:
    void fill_rotations() {
        rotations.reserve(24);

        std::vector<std::function<Point(const Point&)>> base_transforms = {
            [](const Point& p) { return Point{p.x, p.y, p.z}; },
            [](const Point& p) { return Point{p.x, -p.y, -p.z}; },
            [](const Point& p) { return Point{p.x, -p.z, p.y}; },
            [](const Point& p) { return Point{-p.y, -p.z, p.x}; },
            [](const Point& p) { return Point{-p.x, -p.z, -p.y}; },
            [](const Point& p) { return Point{p.y, -p.z, -p.x}; }
        };

        std::vector<std::function<Point(const Point&)>> z_rotations = {
            [](const Point& p) { return Point{p.x, p.y, p.z}; },
            [](const Point& p) { return Point{-p.y, p.x, p.z}; },
            [](const Point& p) { return Point{-p.x, -p.y, p.z}; },
            [](const Point& p) { return Point{p.y, -p.x, p.z}; }
        };

        for (const auto& base_transform : base_transforms) {
            std::vector<Point> current_base_coords;
            current_base_coords.reserve(relative_coords.size());
            for (const auto& p : relative_coords) {
                current_base_coords.push_back(base_transform(p));
            }

            for (const auto& z_rot : z_rotations) {
                std::vector<Point> full_rotation;
                full_rotation.reserve(relative_coords.size());
                for (const auto& p_transformed : current_base_coords) {
                    full_rotation.push_back(z_rot(p_transformed));
                }
                rotations.push_back(full_rotation);
            }
        }
    }
};

std::vector<Point> make_absolute_coords_list(const Point& absolute, const Point& relative, const std::vector<Point>& relative_coords_list) {
    Point diff = {absolute.x - relative.x, absolute.y - relative.y, absolute.z - relative.z};
    std::vector<Point> result;
    result.reserve(relative_coords_list.size());
    for (const auto& c : relative_coords_list) {
        result.push_back({diff.x + c.x, diff.y + c.y, diff.z + c.z});
    }
    return result;
}

bool find_absolute_coords_for_scanner(Scanner& undet, const std::vector<Scanner>& settled) {
    for (const auto& rotated_coords : undet.rotations) {
        for (const auto& set_scanner : settled) {
            for (const auto& abs_coord_from_set : set_scanner.absolute_coords) {
                for (const auto& relative_coord_from_undet : rotated_coords) {
                    std::vector<Point> hypothesized_absolute_coords = make_absolute_coords_list(
                        abs_coord_from_set, relative_coord_from_undet, rotated_coords);

                    int matching_count = 0;
                    for (const auto& ac : hypothesized_absolute_coords) {
                        if (set_scanner.absolute_coords_map.count(ac)) {
                            matching_count++;
                        }
                    }

                    if (matching_count >= 12) {
                        undet.relative_coords = rotated_coords;
                        undet.absolute_coords = hypothesized_absolute_coords;
                        undet.fill_absolute_coords_map();
                        undet.x = abs_coord_from_set.x - relative_coord_from_undet.x;
                        undet.y = abs_coord_from_set.y - relative_coord_from_undet.y;
                        undet.z = abs_coord_from_set.z - relative_coord_from_undet.z;
                        return true;
                    }
                }
            }
        }
    }
    return false;
}

std::vector<Scanner> parse_input(const std::string& filename) {
    std::vector<Scanner> scanners;
    std::ifstream file(filename);
    std::string line;
    std::stringstream current_scanner_block;

    int current_scanner_number = 0;

    while (std::getline(file, line)) {
        if (line.empty()) {
            if (current_scanner_block.tellp() > 0) {
                std::string header_line;
                std::getline(current_scanner_block, header_line);
                
                std::stringstream ss_header(header_line);
                std::string token;
                ss_header >> token >> token >> current_scanner_number;

                std::vector<Point> coords;
                while (std::getline(current_scanner_block, line)) {
                    std::stringstream ss_line(line);
                    std::string x_str, y_str, z_str;
                    std::getline(ss_line, x_str, ',');
                    std::getline(ss_line, y_str, ',');
                    std::getline(ss_line, z_str);
                    coords.push_back({std::stoi(x_str), std::stoi(y_str), std::stoi(z_str)});
                }
                scanners.emplace_back(current_scanner_number, coords);
            }
            current_scanner_block.str("");
            current_scanner_block.clear();
        } else {
            current_scanner_block << line << "\n";
        }
    }
    if (current_scanner_block.tellp() > 0) {
        std::string header_line;
        std::getline(current_scanner_block, header_line);
        std::stringstream ss_header(header_line);
        std::string token;
        ss_header >> token >> token >> current_scanner_number;

        std::vector<Point> coords;
        while (std::getline(current_scanner_block, line)) {
            std::stringstream ss_line(line);
            std::string x_str, y_str, z_str;
            std::getline(ss_line, x_str, ',');
            std::getline(ss_line, y_str, ',');
            std::getline(ss_line, z_str);
            coords.push_back({std::stoi(x_str), std::stoi(y_str), std::stoi(z_str)});
        }
        scanners.emplace_back(current_scanner_number, coords);
    }

    return scanners;
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::vector<Scanner> scanners = parse_input("input.txt");

    std::vector<Scanner> settled_scanners;
    settled_scanners.push_back(scanners[0]);
    settled_scanners[0].absolute_coords = settled_scanners[0].relative_coords;
    settled_scanners[0].fill_absolute_coords_map();

    std::vector<Scanner> undetermined_scanners;
    for (size_t i = 1; i < scanners.size(); ++i) {
        undetermined_scanners.push_back(scanners[i]);
    }

    while (!undetermined_scanners.empty()) {
        bool scanner_found_this_round = false;
        for (size_t i = 0; i < undetermined_scanners.size(); ++i) {
            if (find_absolute_coords_for_scanner(undetermined_scanners[i], settled_scanners)) {
                settled_scanners.push_back(undetermined_scanners[i]);
                undetermined_scanners.erase(undetermined_scanners.begin() + i);
                scanner_found_this_round = true;
                break;
            }
        }
        if (!scanner_found_this_round && !undetermined_scanners.empty()) {
            break;
        }
    }

    std::set<Point> all_beacons;
    for (const auto& s : settled_scanners) {
        for (const auto& p : s.absolute_coords) {
            all_beacons.insert(p);
        }
    }

    std::cout << all_beacons.size() << std::endl;

    return 0;
}
