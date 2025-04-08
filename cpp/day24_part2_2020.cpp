
#include <iostream>
#include <fstream>
#include <string>
#include <unordered_map>

using namespace std;

int main() {
    unordered_map<long long, bool> tiles;

    ifstream file("input.txt");
    string line;

    while (getline(file, line)) {
        long long x = 0, y = 0;
        for (size_t i = 0; i < line.length();) {
            if (line[i] == 'e') {
                x++;
                i++;
            } else if (line[i] == 'w') {
                x--;
                i++;
            } else if (line[i] == 's' && line[i+1] == 'e') {
                y--;
                i += 2;
            } else if (line[i] == 's' && line[i+1] == 'w') {
                x--;
                y--;
                i += 2;
            } else if (line[i] == 'n' && line[i+1] == 'w') {
                y++;
                i += 2;
            } else if (line[i] == 'n' && line[i+1] == 'e') {
                x++;
                y++;
                i += 2;
            }
        }
        long long key = (x << 32) | (y & 0xFFFFFFFF);
        tiles[key] = !tiles[key];
    }

    for (int day = 0; day < 100; ++day) {
        unordered_map<long long, bool> new_tiles;
        unordered_map<long long, bool> to_check = tiles;

        for (auto const& [key, val] : tiles) {
            long long x = key >> 32;
            long long y = key & 0xFFFFFFFF;

            to_check[(x << 32) | (y & 0xFFFFFFFF)] = true;
            to_check[((x + 1) << 32) | (y & 0xFFFFFFFF)] = true;
            to_check[((x - 1) << 32) | (y & 0xFFFFFFFF)] = true;
            to_check[(x << 32) | ((y - 1) & 0xFFFFFFFF)] = true;
            to_check[((x - 1) << 32) | ((y - 1) & 0xFFFFFFFF)] = true;
            to_check[(x << 32) | ((y + 1) & 0xFFFFFFFF)] = true;
            to_check[((x + 1) << 32) | ((y + 1) & 0xFFFFFFFF)] = true;
        }

        for(auto const& [key, _] : to_check){
            long long x = key >> 32;
            long long y = key & 0xFFFFFFFF;

            int adj_count = 0;
            long long neighbor_key;

            neighbor_key = ((x + 1) << 32) | (y & 0xFFFFFFFF);
            adj_count += tiles[neighbor_key];

            neighbor_key = ((x - 1) << 32) | (y & 0xFFFFFFFF);
            adj_count += tiles[neighbor_key];

            neighbor_key = (x << 32) | ((y - 1) & 0xFFFFFFFF);
            adj_count += tiles[neighbor_key];

            neighbor_key = ((x - 1) << 32) | ((y - 1) & 0xFFFFFFFF);
            adj_count += tiles[neighbor_key];

            neighbor_key = (x << 32) | ((y + 1) & 0xFFFFFFFF);
            adj_count += tiles[neighbor_key];

            neighbor_key = ((x + 1) << 32) | ((y + 1) & 0xFFFFFFFF);
            adj_count += tiles[neighbor_key];

            long long current_key = (x << 32) | (y & 0xFFFFFFFF);
            if (tiles[current_key]) {
                new_tiles[current_key] = (adj_count == 1 || adj_count == 2);
            } else {
                new_tiles[current_key] = (adj_count == 2);
            }
        }
        tiles = new_tiles;
    }

    int count = 0;
    for (auto const& [key, val] : tiles) {
        if (val) {
            count++;
        }
    }

    cout << count << endl;

    return 0;
}
