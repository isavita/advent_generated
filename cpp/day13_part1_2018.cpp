
#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <map>
#include <algorithm>
#include <utility>

enum TurnState { LEFT, STRAIGHT, RIGHT };

struct Cart {
    int id;
    int x, y;
    char direction;
    TurnState next_turn;
};

std::vector<std::string> track_layout;
std::map<int, Cart> carts;
std::vector<std::vector<int>> occupied_grid;

std::map<char, std::pair<int, int>> directions_map;
std::map<char, char> turns_left_map;
std::map<char, char> turns_right_map;

void init_static_data() {
    directions_map['^'] = {0, -1};
    directions_map['v'] = {0, 1};
    directions_map['<'] = {-1, 0};
    directions_map['>'] = {1, 0};

    turns_left_map['^'] = '<';
    turns_left_map['<'] = 'v';
    turns_left_map['v'] = '>';
    turns_left_map['>'] = '^';

    turns_right_map['^'] = '>';
    turns_right_map['<'] = '^';
    turns_right_map['v'] = '<';
    turns_right_map['>'] = 'v';
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    init_static_data();

    std::ifstream infile("input.txt");
    std::string line;

    int max_x = 0;
    while (std::getline(infile, line)) {
        track_layout.push_back(line);
        if (line.length() > max_x) {
            max_x = line.length();
        }
    }
    infile.close();

    int max_y = track_layout.size();
    occupied_grid.resize(max_y, std::vector<int>(max_x, 0));

    int cart_id_counter = 1;
    for (int y = 0; y < max_y; ++y) {
        for (int x = 0; x < track_layout[y].length(); ++x) {
            char c = track_layout[y][x];
            if (directions_map.count(c)) {
                carts[cart_id_counter] = {cart_id_counter, x, y, c, LEFT};
                occupied_grid[y][x] = cart_id_counter;

                if (c == '^' || c == 'v') track_layout[y][x] = '|';
                else if (c == '<' || c == '>') track_layout[y][x] = '-';
                
                cart_id_counter++;
            }
        }
    }

    while (true) {
        std::vector<int> sorted_cart_ids;
        for (const auto& pair : carts) {
            sorted_cart_ids.push_back(pair.first);
        }

        std::sort(sorted_cart_ids.begin(), sorted_cart_ids.end(),
                  [&](int id1, int id2) {
                      const Cart& c1 = carts.at(id1);
                      const Cart& c2 = carts.at(id2);
                      if (c1.y != c2.y) {
                          return c1.y < c2.y;
                      }
                      return c1.x < c2.x;
                  });

        for (int current_cart_id : sorted_cart_ids) {
            if (carts.find(current_cart_id) == carts.end()) {
                continue;
            }

            Cart current_cart = carts.at(current_cart_id);
            
            occupied_grid[current_cart.y][current_cart.x] = 0;
            
            int dx = directions_map[current_cart.direction].first;
            int dy = directions_map[current_cart.direction].second;
            current_cart.x += dx;
            current_cart.y += dy;

            if (occupied_grid[current_cart.y][current_cart.x] != 0) {
                std::cout << current_cart.x << "," << current_cart.y << std::endl;
                return 0;
            }
            
            char track_char = track_layout[current_cart.y][current_cart.x];
            if (track_char == '+') {
                if (current_cart.next_turn == LEFT) {
                    current_cart.direction = turns_left_map[current_cart.direction];
                    current_cart.next_turn = STRAIGHT;
                } else if (current_cart.next_turn == STRAIGHT) {
                    current_cart.next_turn = RIGHT;
                } else if (current_cart.next_turn == RIGHT) {
                    current_cart.direction = turns_right_map[current_cart.direction];
                    current_cart.next_turn = LEFT;
                }
            } else if (track_char == '/') {
                if (current_cart.direction == '>') current_cart.direction = '^';
                else if (current_cart.direction == 'v') current_cart.direction = '<';
                else if (current_cart.direction == '<') current_cart.direction = 'v';
                else if (current_cart.direction == '^') current_cart.direction = '>';
            } else if (track_char == '\\') {
                if (current_cart.direction == '>') current_cart.direction = 'v';
                else if (current_cart.direction == 'v') current_cart.direction = '>';
                else if (current_cart.direction == '<') current_cart.direction = '^';
                else if (current_cart.direction == '^') current_cart.direction = '<';
            }

            carts[current_cart_id] = current_cart;
            occupied_grid[current_cart.y][current_cart.x] = current_cart_id;
        }
    }

    return 0;
}
