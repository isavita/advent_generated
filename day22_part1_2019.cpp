
#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <sstream>

const int Size = 10007;

std::vector<int> dealIntoNewStack(std::vector<int> deck) {
    for (int i = 0; i < Size / 2; i++) {
        std::swap(deck[i], deck[Size - i - 1]);
    }
    return deck;
}

std::vector<int> cutN(std::vector<int> deck, int n) {
    if (n >= 0) {
        std::rotate(deck.begin(), deck.begin() + n, deck.end());
    } else {
        std::rotate(deck.rbegin(), deck.rbegin() - n, deck.rend());
    }
    return deck;
}

std::vector<int> dealWithIncrement(std::vector<int> deck, int n) {
    std::vector<int> newDeck(Size);

    for (int i = 0; i < Size; i++) {
        newDeck[(i * n) % Size] = deck[i];
    }

    return newDeck;
}

int find2019(std::vector<int> deck) {
    for (int i = 0; i < Size; i++) {
        if (deck[i] == 2019) {
            return i;
        }
    }
    return -1;
}

int main() {
    std::vector<int> deck(Size);
    for (int i = 0; i < Size; i++) {
        deck[i] = i;
    }

    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cerr << "Error opening file" << std::endl;
        return 1;
    }

    std::string line;
    while (std::getline(file, line)) {
        if (line == "deal into new stack") {
            deck = dealIntoNewStack(deck);
            continue;
        }

        if (line.find("cut") == 0) {
            int n;
            std::istringstream iss(line);
            iss >> line >> n;
            deck = cutN(deck, n);
            continue;
        }

        if (line.find("deal with increment") == 0) {
            int n;
            std::istringstream iss(line);
            std::string temp;
            while (iss >> temp) {
                if (std::stringstream(temp) >> n) {
                    break;
                }
            }
            deck = dealWithIncrement(deck, n);
            continue;
        }
    }

    std::cout << find2019(deck) << std::endl;

    return 0;
}
