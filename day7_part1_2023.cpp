
#include <iostream>
#include <fstream>
#include <vector>
#include <regex>
#include <algorithm>
#include <unordered_map>

const int HighCard = 1;
const int OnePair = 2;
const int TwoPair = 3;
const int ThreeKind = 4;
const int FullHouse = 5;
const int FourKind = 6;
const int FiveKind = 7;

struct Hand {
    std::string cards;
    int bid;
};

struct RankedHand {
    Hand hand;
    int rank;
};

std::vector<Hand> matches[7];

void findMatches(std::vector<Hand>& hands) {
    for (auto& hand : hands) {
        std::unordered_map<char, int> count;

        for (char card : hand.cards) {
            count[card]++;
        }

        int value = 1;
        for (auto& c : count) {
            value *= c.second;
        }

        switch (value) {
            case 1:
                matches[6].push_back(hand);
                break;
            case 2:
                matches[5].push_back(hand);
                break;
            case 3:
                matches[3].push_back(hand);
                break;
            case 4:
                if (count.size() == 2) {
                    matches[1].push_back(hand);
                } else {
                    matches[4].push_back(hand);
                }
                break;
            case 5:
                matches[0].push_back(hand);
                break;
            case 6:
                matches[2].push_back(hand);
                break;
            default:
                std::cout << "oh no" << std::endl;
        }
    }
}

std::vector<RankedHand> convertAndOrderMatches() {
    std::vector<RankedHand> convertedMatches;

    for (auto& category : matches) {
        std::vector<RankedHand> temp;

        for (auto& hand : category) {
            std::string cards = std::regex_replace(hand.cards, std::regex("A"), "E");
            cards = std::regex_replace(cards, std::regex("T"), "A");
            cards = std::regex_replace(cards, std::regex("J"), "B");
            cards = std::regex_replace(cards, std::regex("Q"), "C");
            cards = std::regex_replace(cards, std::regex("K"), "D");

            int num = std::stoi(cards, nullptr, 16);

            temp.push_back({hand, num});
        }

        std::sort(temp.begin(), temp.end(), [](const RankedHand& a, const RankedHand& b) {
            return a.rank > b.rank;
        });

        convertedMatches.insert(convertedMatches.end(), temp.begin(), temp.end());
    }

    return convertedMatches;
}

int main() {
    std::ifstream inputFile("input.txt");
    if (!inputFile.is_open()) {
        std::cerr << "Error opening input file" << std::endl;
        return 1;
    }

    std::string line;
    std::vector<Hand> hands;

    while (std::getline(inputFile, line)) {
        if (line.empty()) {
            continue;
        }

        std::regex re("[\\dAKQJT]+");
        std::regex bidRe(" [\\d]+");

        std::smatch match;
        std::regex_search(line, match, re);
        std::string cards = match.str();

        std::regex_search(line, match, bidRe);
        int bid = std::stoi(match.str().substr(1));

        hands.push_back({cards, bid});
    }

    findMatches(hands);

    std::vector<RankedHand> convertedMatches = convertAndOrderMatches();

    int total = 0;
    for (int i = 0; i < convertedMatches.size(); i++) {
        total += convertedMatches[i].hand.bid * (convertedMatches.size() - i);
    }

    std::cout << total << std::endl;

    return 0;
}
