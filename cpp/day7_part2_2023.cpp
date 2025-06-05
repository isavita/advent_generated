
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <map>
#include <algorithm>
#include <iomanip>

static std::map<char, int> card_values_joker_rules;
static std::map<char, char> hex_map;

void init_maps() {
    if (card_values_joker_rules.empty()) {
        card_values_joker_rules['J'] = 1;
        card_values_joker_rules['2'] = 2;
        card_values_joker_rules['3'] = 3;
        card_values_joker_rules['4'] = 4;
        card_values_joker_rules['5'] = 5;
        card_values_joker_rules['6'] = 6;
        card_values_joker_rules['7'] = 7;
        card_values_joker_rules['8'] = 8;
        card_values_joker_rules['9'] = 9;
        card_values_joker_rules['T'] = 10;
        card_values_joker_rules['Q'] = 11;
        card_values_joker_rules['K'] = 12;
        card_values_joker_rules['A'] = 13;
    }
    if (hex_map.empty()) {
        hex_map['A'] = 'E';
        hex_map['T'] = 'A';
        hex_map['J'] = '1';
        hex_map['Q'] = 'C';
        hex_map['K'] = 'D';
    }
}

struct Hand {
    std::string cards;
    int bid;
    int hand_type;
    unsigned long long hex_val;

    void calculate_hex_val() {
        std::string temp_cards = cards;
        for (char& c : temp_cards) {
            if (hex_map.count(c)) {
                c = hex_map.at(c);
            }
        }
        hex_val = std::stoull(temp_cards, nullptr, 16);
    }
};

void classify_hand(Hand& h) {
    std::map<char, int> counts;
    for (char c : h.cards) {
        counts[c]++;
    }

    if (counts.count('J') && counts.at('J') > 0) {
        int joker_count = counts.at('J');
        counts.erase('J');

        if (counts.empty()) {
            counts['J'] = joker_count;
        } else {
            char high_key = ' ';
            int high_v = -1; // Initialize with -1 to ensure any count is higher

            for (auto const& [card, count] : counts) {
                if (count > high_v) {
                    high_key = card;
                    high_v = count;
                } else if (count == high_v) {
                    if (card_values_joker_rules.at(card) > card_values_joker_rules.at(high_key)) {
                        high_key = card;
                    }
                }
            }
            counts[high_key] += joker_count;
        }
    }

    int value_product = 1;
    for (auto const& pair : counts) {
        value_product *= pair.second;
    }

    int distinct_cards = counts.size();

    if (value_product == 1 && distinct_cards == 5) { // High Card
        h.hand_type = 6;
    } else if (value_product == 2 && distinct_cards == 4) { // One Pair
        h.hand_type = 5;
    } else if (value_product == 3 && distinct_cards == 3) { // Three of a Kind
        h.hand_type = 3;
    } else if (value_product == 4) {
        if (distinct_cards == 2) { // Four of a Kind
            h.hand_type = 1;
        } else { // Two Pair
            h.hand_type = 4;
        }
    } else if (value_product == 5 && distinct_cards == 1) { // Five of a Kind
        h.hand_type = 0;
    } else if (value_product == 6 && distinct_cards == 2) { // Full House
        h.hand_type = 2;
    }
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    init_maps();

    std::ifstream file("input.txt");
    if (!file.is_open()) {
        return 1;
    }

    std::vector<Hand> hands;
    std::string line;
    while (std::getline(file, line)) {
        if (line.empty()) continue;

        size_t space_pos = line.find(' ');
        Hand h;
        h.cards = line.substr(0, space_pos);
        h.bid = std::stoi(line.substr(space_pos + 1));
        h.calculate_hex_val();
        hands.push_back(h);
    }
    file.close();

    std::vector<std::vector<Hand>> matches(7);

    for (Hand& hand : hands) {
        classify_hand(hand);
        matches[hand.hand_type].push_back(hand);
    }

    std::vector<Hand> final_sorted_hands;
    for (int i = 0; i < 7; ++i) {
        std::sort(matches[i].begin(), matches[i].end(), [](const Hand& a, const Hand& b) {
            return a.hex_val > b.hex_val;
        });
        final_sorted_hands.insert(final_sorted_hands.end(), matches[i].begin(), matches[i].end());
    }

    long long total_winnings = 0;
    for (int i = 0; i < final_sorted_hands.size(); ++i) {
        total_winnings += (long long)final_sorted_hands[i].bid * (final_sorted_hands.size() - i);
    }

    std::cout << total_winnings << std::endl;

    return 0;
}
