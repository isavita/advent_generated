#include <fstream>
#include <iostream>
#include <string>
#include <vector>
#include <algorithm>

using namespace std;

struct GameState {
    int playerHP, playerMana, bossHP, bossDamage;
    int shieldTimer, poisonTimer, rechargeTimer;
    int manaSpent;
};

int minManaToWin(GameState state, bool playerTurn) {
    static int minMana = INT_MAX;
    if (state.manaSpent >= minMana) return minMana;
    if (state.bossHP <= 0) {
        minMana = min(minMana, state.manaSpent);
        return minMana;
    }
    if (state.playerHP <= 0) return minMana;

    if (playerTurn) {
        state.playerHP--;
        if (state.playerHP <= 0) return minMana;
    }

    if (state.shieldTimer > 0) state.shieldTimer--;
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
        if (state.shieldTimer > 0) damage -= 7;
        if (damage < 1) damage = 1;
        state.playerHP -= damage;
        minManaToWin(state, true);
        return minMana;
    }

    if (state.playerMana >= 53) {
        GameState newState = state;
        newState.playerMana -= 53;
        newState.manaSpent += 53;
        newState.bossHP -= 4;
        minManaToWin(newState, false);
    }
    if (state.playerMana >= 73) {
        GameState newState = state;
        newState.playerMana -= 73;
        newState.manaSpent += 73;
        newState.bossHP -= 2;
        newState.playerHP += 2;
        minManaToWin(newState, false);
    }
    if (state.playerMana >= 113 && state.shieldTimer == 0) {
        GameState newState = state;
        newState.playerMana -= 113;
        newState.manaSpent += 113;
        newState.shieldTimer = 6;
        minManaToWin(newState, false);
    }
    if (state.playerMana >= 173 && state.poisonTimer == 0) {
        GameState newState = state;
        newState.playerMana -= 173;
        newState.manaSpent += 173;
        newState.poisonTimer = 6;
        minManaToWin(newState, false);
    }
    if (state.playerMana >= 229 && state.rechargeTimer == 0) {
        GameState newState = state;
        newState.playerMana -= 229;
        newState.manaSpent += 229;
        newState.rechargeTimer = 5;
        minManaToWin(newState, false);
    }
    return minMana;
}

int main() {
    ifstream file("input.txt");
    string line;
    getline(file, line);
    int bossHP = stoi(line.substr(line.find(": ") + 2));
    getline(file, line);
    int bossDamage = stoi(line.substr(line.find(": ") + 2));

    GameState initialState = {50, 500, bossHP, bossDamage, 0, 0, 0, 0};
    cout << minManaToWin(initialState, true) << endl;
    return 0;
}