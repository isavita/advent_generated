#include <iostream>
#include <fstream>
#include <vector>
#include <string>

int main() {
    std::ifstream file("input.txt");
    std::vector<int> player1Deck, player2Deck;
    std::vector<int>* currentDeck = &player1Deck;
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
        currentDeck->push_back(card);
    }

    while (!player1Deck.empty() && !player2Deck.empty()) {
        int card1 = player1Deck[0];
        int card2 = player2Deck[0];
        player1Deck.erase(player1Deck.begin());
        player2Deck.erase(player2Deck.begin());
        if (card1 > card2) {
            player1Deck.push_back(card1);
            player1Deck.push_back(card2);
        } else {
            player2Deck.push_back(card2);
            player2Deck.push_back(card1);
        }
    }

    std::vector<int> winningDeck;
    if (!player1Deck.empty()) {
        winningDeck = player1Deck;
    } else {
        winningDeck = player2Deck;
    }

    int score = 0;
    for (int i = 0; i < winningDeck.size(); i++) {
        score += winningDeck[i] * (winningDeck.size() - i);
    }
    std::cout << score << std::endl;

    return 0;
}