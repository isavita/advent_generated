
#include <iostream>
#include <vector>
#include <string>
#include <map>
#include <algorithm>
#include <fstream>
#include <utility>

struct Cart {
    int x, y;
    char direction;
    int turn_state;

    bool operator<(const Cart& other) const {
        if (y != other.y) return y < other.y;
        return x < other.x;
    }
};

const std::map<char, std::pair<int, int>> direction_deltas = {
    {'^', {0, -1}},
    {'v', {0, 1}},
    {'<', {-1, 0}},
    {'>', {1, 0}}
};

const std::map<char, std::map<char, char>> curve_slash_turns = {
    {'^', {{'/', '>'}, {'\\', '<'}}},
    {'v', {{'/', '<'}, {'\\', '>'}}},
    {'<', {{'/', 'v'}, {'\\', '^'}}},
    {'>', {{'/', '^'}, {'\\', 'v'}}}
};

const std::map<char, std::map<int, char>> intersection_turns = {
    {'^', {{0, '<'}, {1, '^'}, {2, '>'}}},
    {'v', {{0, '>'}, {1, 'v'}, {2, '<'}}},
    {'<', {{0, 'v'}, {1, '<'}, {2, '^'}}},
    {'>', {{0, '^'}, {1, '>'}, {2, 'v'}}}
};

void parseInput(const std::string& filePath, std::map<std::pair<int, int>, char>& tracks, std::vector<Cart>& carts) {
    std::ifstream file(filePath);
    std::string line;
    int y = 0;
    while (std::getline(file, line)) {
        for (int x = 0; x < line.length(); ++x) {
            char char_ = line[x];
            if (char_ == '^' || char_ == 'v' || char_ == '<' || char_ == '>') {
                carts.push_back({x, y, char_, 0});
                if (char_ == '^' || char_ == 'v') {
                    tracks[{x, y}] = '|';
                } else {
                    tracks[{x, y}] = '-';
                }
            } else if (char_ == '|' || char_ == '-' || char_ == '+' || char_ == '/' || char_ == '\\') {
                tracks[{x, y}] = char_;
            }
        }
        y++;
    }
}

void moveCart(Cart& cart, const std::map<std::pair<int, int>, char>& tracks) {
    int dx = direction_deltas.at(cart.direction).first;
    int dy = direction_deltas.at(cart.direction).second;

    cart.x += dx;
    cart.y += dy;

    char track_char = tracks.at({cart.x, cart.y});
    
    if (track_char == '/') {
        cart.direction = curve_slash_turns.at(cart.direction).at('/');
    } else if (track_char == '\\') {
        cart.direction = curve_slash_turns.at(cart.direction).at('\\');
    } else if (track_char == '+') {
        cart.direction = intersection_turns.at(cart.direction).at(cart.turn_state);
        cart.turn_state = (cart.turn_state + 1) % 3;
    }
}

std::pair<int, int> simulate(std::map<std::pair<int, int>, char>& tracks, std::vector<Cart>& carts) {
    while (carts.size() > 1) {
        std::sort(carts.begin(), carts.end());

        std::vector<bool> active_carts(carts.size(), true);
        
        for (int i = 0; i < carts.size(); ++i) {
            if (!active_carts[i]) {
                continue;
            }

            moveCart(carts[i], tracks);

            for (int j = 0; j < carts.size(); ++j) {
                if (i == j || !active_carts[j]) {
                    continue;
                }
                if (carts[i].x == carts[j].x && carts[i].y == carts[j].y) {
                    active_carts[i] = false;
                    active_carts[j] = false;
                    break;
                }
            }
        }

        std::vector<Cart> next_carts;
        for (int i = 0; i < carts.size(); ++i) {
            if (active_carts[i]) {
                next_carts.push_back(carts[i]);
            }
        }
        carts = next_carts;
    }
    return {carts[0].x, carts[0].y};
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::map<std::pair<int, int>, char> tracks;
    std::vector<Cart> carts;

    parseInput("input.txt", tracks, carts);

    std::pair<int, int> last_cart_position = simulate(tracks, carts);

    std::cout << last_cart_position.first << "," << last_cart_position.second << std::endl;

    return 0;
}
