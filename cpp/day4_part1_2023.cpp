
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <sstream>
#include <algorithm>

using namespace std;

int calculate_points(const vector<int>& winning_numbers, const vector<int>& your_numbers) {
    int points = 0;
    for (int num : your_numbers) {
        if (find(winning_numbers.begin(), winning_numbers.end(), num) != winning_numbers.end()) {
            points = (points == 0) ? 1 : points * 2;
        }
    }
    return points;
}

int main() {
    ifstream file("input.txt");
    string line;
    int total_points = 0;

    while (getline(file, line)) {
        size_t delimiter_pos = line.find(" | ");
        if (delimiter_pos != string::npos) {
            string winning_part = line.substr(0, delimiter_pos);
            string your_part = line.substr(delimiter_pos + 3);

            vector<int> winning_numbers;
            stringstream winning_stream(winning_part);
            string num_str;
            while (winning_stream >> num_str) {
                if (all_of(num_str.begin(), num_str.end(), ::isdigit)) {
                    winning_numbers.push_back(stoi(num_str));
                }
            }

            vector<int> your_numbers;
            stringstream your_stream(your_part);
            while (your_stream >> num_str) {
                if (all_of(num_str.begin(), num_str.end(), ::isdigit)) {
                    your_numbers.push_back(stoi(num_str));
                }
            }

            total_points += calculate_points(winning_numbers, your_numbers);
        }
    }

    cout << total_points << endl;

    return 0;
}
