
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <set>
#include <utility> // For std::pair

const int CHAMBER_WIDTH = 7;
const int FLOOR_Y = 0;

struct Point {
    int x, y;

    // Required for std::set to order elements
    bool operator<(const Point& other) const {
        if (x != other.x) return x < other.x;
        return y < other.y;
    }
};

std::vector<std::vector<Point>> get_rock_shapes() {
    return {
        // Horizontal Line (-)
        {{0,0}, {1,0}, {2,0}, {3,0}},
        // Plus (+)
        {{1,0}, {0,1}, {1,1}, {2,1}, {1,2}},
        // Inverted L (⌝)
        {{0,0}, {1,0}, {2,0}, {2,1}, {2,2}},
        // Vertical Line (|)
        {{0,0}, {0,1}, {0,2}, {0,3}},
        // Square (■)
        {{0,0}, {1,0}, {0,1}, {1,1}}
    };
}

// Checks if a given rock configuration collides with boundaries or settled rocks
bool check_collision(const std::vector<Point>& rock_coords, const std::set<Point>& chamber) {
    for (const auto& p : rock_coords) {
        // Check chamber boundaries (0 to 6 for x, y > 0 for actual chamber as y=0 is floor)
        if (p.x < 0 || p.x >= CHAMBER_WIDTH || p.y <= FLOOR_Y) {
            return true; // Collision with wall or floor
        }
        // Check for collision with settled rocks
        if (chamber.count(p)) {
            return true; // Collision with a settled rock
        }
    }
    return false; // No collision
}

// Moves a rock by (dx, dy) and returns the new configuration
std::vector<Point> move_rock(const std::vector<Point>& current_rock, int dx, int dy) {
    std::vector<Point> new_rock;
    for (const auto& p : current_rock) {
        new_rock.push_back({p.x + dx, p.y + dy});
    }
    return new_rock;
}

int main() {
    std::ifstream file("input.txt");
    std::string jet_pattern;
    std::getline(file, jet_pattern);

    auto rock_shapes = get_rock_shapes();
    std::set<Point> chamber;
    
    // Initialize the chamber with the floor at y = 0
    for (int x = 0; x < CHAMBER_WIDTH; ++x) {
        chamber.insert({x, FLOOR_Y});
    }
    
    int highest_y = 0; // Tracks the highest y-coordinate in the chamber
    int jet_index = 0;
    const int total_rocks = 2022;
    const int jet_len = jet_pattern.length();
    const int num_rock_shapes = rock_shapes.size();

    for (int rock_number = 0; rock_number < total_rocks; ++rock_number) {
        // Determine the current rock shape
        const auto& current_shape_template = rock_shapes[rock_number % num_rock_shapes];
        
        // Starting position: left edge 2 units from the left wall, bottom edge 3 units above the highest rock
        int rock_x_offset = 2;
        int rock_y_offset = highest_y + 4; 
        
        std::vector<Point> current_rock;
        for (const auto& p : current_shape_template) {
            current_rock.push_back({rock_x_offset + p.x, rock_y_offset + p.y});
        }
        
        while (true) {
            // Apply jet push
            char jet_dir = jet_pattern[jet_index % jet_len];
            jet_index++;
            
            int dx = 0;
            if (jet_dir == '>') {
                dx = 1;
            } else if (jet_dir == '<') {
                dx = -1;
            }
            
            std::vector<Point> attempted_jet_rock = move_rock(current_rock, dx, 0);
            if (!check_collision(attempted_jet_rock, chamber)) {
                current_rock = attempted_jet_rock;
            }
            
            // Attempt to move down
            std::vector<Point> attempted_fall_rock = move_rock(current_rock, 0, -1);
            if (!check_collision(attempted_fall_rock, chamber)) {
                current_rock = attempted_fall_rock;
            } else {
                // Rock comes to rest
                for (const auto& p : current_rock) {
                    chamber.insert(p);
                    if (p.y > highest_y) {
                        highest_y = p.y;
                    }
                }
                break; // Move to the next rock
            }
        }
    }
    
    std::cout << highest_y << std::endl;

    return 0;
}
