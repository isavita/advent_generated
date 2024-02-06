#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>

struct GameState {
    int playerHP, playerMana, bossHP, bossDamage;
    int shieldTimer, poisonTimer, rechargeTimer;
    int manaSpent;
};

void simulate(GameState state, bool playerTurn, int& minMana);

int minManaToWin(GameState initialState) {
    int minMana = INT_MAX;
    simulate(initialState, true, minMana);
    return minMana;
}

void simulate(GameState state, bool playerTurn, int& minMana) {
    if (state.manaSpent >= minMana) {
        return;
    }
    if (state.bossHP <= 0) {
        minMana = state.manaSpent;
        return;
    }
    if (state.playerHP <= 0) {
        return;
    }

    // Apply effects
    if (state.shieldTimer > 0) {
        state.shieldTimer--;
    }
    if (state.poisonTimer > 0) {
        state.bossHP -= 3;
        state.poisonTimer--;
    }
    if (state.rechargeTimer > 0) {
        state.playerMana += 101;
        state.rechargeTimer--;
    }

    if (!playerTurn) {
        int damage = state.bossDamage;
        if (state.shieldTimer > 0) {
            damage -= 7;
        }
        if (damage < 1) {
            damage = 1;
        }
        state.playerHP -= damage;
        simulate(state, true, minMana);
        return;
    }

    if (state.playerMana >= 53) {
        GameState newState = state;
        newState.playerMana -= 53;
        newState.manaSpent += 53;
        newState.bossHP -= 4;
        simulate(newState, false, minMana);
    }
    if (state.playerMana >= 73) {
        GameState newState = state;
        newState.playerMana -= 73;
        newState.manaSpent += 73;
        newState.bossHP -= 2;
        newState.playerHP += 2;
        simulate(newState, false, minMana);
    }
    if (state.playerMana >= 113 && state.shieldTimer == 0) {
        GameState newState = state;
        newState.playerMana -= 113;
        newState.manaSpent += 113;
        newState.shieldTimer = 6;
        simulate(newState, false, minMana);
    }
    if (state.playerMana >= 173 && state.poisonTimer == 0) {
        GameState newState = state;
        newState.playerMana -= 173;
        newState.manaSpent += 173;
        newState.poisonTimer = 6;
        simulate(newState, false, minMana);
    }
    if (state.playerMana >= 229 && state.rechargeTimer == 0) {
        GameState newState = state;
        newState.playerMana -= 229;
        newState.manaSpent += 229;
        newState.rechargeTimer = 5;
        simulate(newState, false, minMana);
    }
}

int main() {
    std::ifstream file("input.txt");
    GameState initialState;
    if (file.is_open()) {
        std::string line;
        std::getline(file, line);
        std::stringstream ss1(line.substr(line.find(": ") + 2));
        ss1 >> initialState.bossHP;
        std::getline(file, line);
        std::stringstream ss2(line.substr(line.find(": ") + 2));
        ss2 >> initialState.bossDamage;
        file.close();
    }

    initialState.playerHP = 50;
    initialState.playerMana = 500;

    std::cout << minManaToWin(initialState) << std::endl;

    return 0;
}