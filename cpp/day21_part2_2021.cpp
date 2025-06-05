
#include <iostream>
#include <fstream>
#include <string>
#include <map>
#include <tuple>
#include <algorithm>
#include <sstream>
#include <iterator>

using GameState = std::tuple<int, int, int, int, bool>;

std::map<GameState, std::pair<long long, long long>> memo;

const std::map<int, int> roll_sums = {
    {3, 1}, {4, 3}, {5, 6}, {6, 7}, {7, 6}, {8, 3}, {9, 1}
};

std::pair<int, int> parse_input(const std::string& input_str) {
    int player1_pos = 0;
    int player2_pos = 0;
    std::istringstream iss(input_str);
    std::string line;

    while (std::getline(iss, line)) {
        if (line.find("Player 1 starting position: ") == 0) {
            player1_pos = std::stoi(line.substr(line.rfind(' ') + 1));
        } else if (line.find("Player 2 starting position: ") == 0) {
            player2_pos = std::stoi(line.substr(line.rfind(' ') + 1));
        }
    }
    return {player1_pos, player2_pos};
}

std::pair<long long, long long> play(GameState state) {
    int pos1, pos2, score1, score2;
    bool is_p1_turn;
    std::tie(pos1, pos2, score1, score2, is_p1_turn) = state;

    if (score1 >= 21) {
        return {1, 0};
    }
    if (score2 >= 21) {
        return {0, 1};
    }

    if (memo.count(state)) {
        return memo[state];
    }

    long long wins1 = 0;
    long long wins2 = 0;

    for (auto const& [roll_sum, ways] : roll_sums) {
        GameState next_state = state;
        if (is_p1_turn) {
            std::get<0>(next_state) = (pos1 + roll_sum - 1) % 10 + 1;
            std::get<2>(next_state) += std::get<0>(next_state);
        } else {
            std::get<1>(next_state) = (pos2 + roll_sum - 1) % 10 + 1;
            std::get<3>(next_state) += std::get<1>(next_state);
        }

        std::get<4>(next_state) = !is_p1_turn;

        std::pair<long long, long long> res = play(next_state);
        wins1 += res.first * ways;
        wins2 += res.second * ways;
    }

    return memo[state] = {wins1, wins2};
}

long long solve(const std::string& input_str) {
    std::pair<int, int> initial_positions = parse_input(input_str);
    GameState initial_state = std::make_tuple(initial_positions.first, initial_positions.second, 0, 0, true);
    std::pair<long long, long long> final_wins = play(initial_state);
    return std::max(final_wins.first, final_wins.second);
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::ifstream ifs("input.txt");
    if (!ifs.is_open()) {
        std::cerr << "Error: Could not open input.txt\n";
        return 1;
    }

    std::string input_str((std::istreambuf_iterator<char>(ifs)),
                           std::istreambuf_iterator<char>());
    ifs.close();

    std::cout << solve(input_str) << std::endl;

    return 0;
}
