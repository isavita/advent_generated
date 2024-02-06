
#include <iostream>
#include <fstream>
#include <vector>
#include <unordered_map>

class Deck {
public:
    std::vector<int> cards;

    Deck Copy(int n) {
        Deck copy;
        for (int i = 0; i < n; i++) {
            copy.cards.push_back(cards[i]);
        }
        return copy;
    }

    int Score() {
        int score = 0;
        for (int i = 0; i < cards.size(); i++) {
            score += cards[i] * (cards.size() - i);
        }
        return score;
    }
};

std::pair<Deck, Deck> playRecursiveCombat(Deck player1, Deck player2) {
    std::unordered_map<std::string, bool> previousRounds;
    while (player1.cards.size() > 0 && player2.cards.size() > 0) {
        std::string roundKey = "";
        for (int card : player1.cards) {
            roundKey += std::to_string(card) + "|";
        }
        roundKey += "|";
        for (int card : player2.cards) {
            roundKey += std::to_string(card) + "|";
        }

        if (previousRounds[roundKey]) {
            return {player1, Deck()};
        }
        previousRounds[roundKey] = true;

        int card1 = player1.cards[0];
        int card2 = player2.cards[0];
        player1.cards.erase(player1.cards.begin());
        player2.cards.erase(player2.cards.begin());

        if (player1.cards.size() >= card1 && player2.cards.size() >= card2) {
            auto subPlayers = playRecursiveCombat(player1.Copy(card1), player2.Copy(card2));
            if (subPlayers.first.cards.size() > 0) {
                player1.cards.push_back(card1);
                player1.cards.push_back(card2);
            } else {
                player2.cards.push_back(card2);
                player2.cards.push_back(card1);
            }
        } else {
            if (card1 > card2) {
                player1.cards.push_back(card1);
                player1.cards.push_back(card2);
            } else {
                player2.cards.push_back(card2);
                player2.cards.push_back(card1);
            }
        }
    }
    return {player1, player2};
}

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cerr << "Error opening file" << std::endl;
        return 1;
    }

    Deck player1Deck, player2Deck;
    Deck* currentDeck = &player1Deck;
    std::string line;
    while (std::getline(file, line)) {
        if (line.empty()) {
            currentDeck = &player2Deck;
            continue;
        }
        if (line.find("Player") != std::string::npos) {
            continue;
        }
        int card = std::stoi(line);
        currentDeck->cards.push_back(card);
    }

    auto winners = playRecursiveCombat(player1Deck, player2Deck);
    Deck winningDeck = (winners.first.cards.size() > 0) ? winners.first : winners.second;

    std::cout << winningDeck.Score() << std::endl;

    return 0;
}
