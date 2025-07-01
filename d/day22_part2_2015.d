
import std.stdio;
import std.file;
import std.string;
import std.conv;
import std.algorithm;
import std.math;

struct GameState {
    int playerHP;
    int playerMana;
    int bossHP;
    int bossDamage;
    int shieldTimer;
    int poisonTimer;
    int rechargeTimer;
    int manaSpent;
}

void main() {
    auto lines = readText("input.txt").strip.split("\n");
    auto bossHP = lines[0].split(": ")[1].to!int;
    auto bossDamage = lines[1].split(": ")[1].to!int;

    int minMana = int.max;

    void simulate(GameState state, bool playerTurn) {
        if (state.manaSpent >= minMana) {
            return;
        }

        if (playerTurn) {
            state.playerHP--;
            if (state.playerHP <= 0) {
                return;
            }
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

        if (state.bossHP <= 0) {
            minMana = min(minMana, state.manaSpent);
            return;
        }

        if (!playerTurn) {
            int damage = state.bossDamage;
            if (state.shieldTimer > 0) {
                damage -= 7;
            }
            state.playerHP -= max(1, damage);
            simulate(state, true);
            return;
        }

        if (state.playerMana >= 53) {
            auto nextState = state;
            nextState.playerMana -= 53;
            nextState.manaSpent += 53;
            nextState.bossHP -= 4;
            simulate(nextState, false);
        }
        if (state.playerMana >= 73) {
            auto nextState = state;
            nextState.playerMana -= 73;
            nextState.manaSpent += 73;
            nextState.bossHP -= 2;
            nextState.playerHP += 2;
            simulate(nextState, false);
        }
        if (state.playerMana >= 113 && state.shieldTimer == 0) {
            auto nextState = state;
            nextState.playerMana -= 113;
            nextState.manaSpent += 113;
            nextState.shieldTimer = 6;
            simulate(nextState, false);
        }
        if (state.playerMana >= 173 && state.poisonTimer == 0) {
            auto nextState = state;
            nextState.playerMana -= 173;
            nextState.manaSpent += 173;
            nextState.poisonTimer = 6;
            simulate(nextState, false);
        }
        if (state.playerMana >= 229 && state.rechargeTimer == 0) {
            auto nextState = state;
            nextState.playerMana -= 229;
            nextState.manaSpent += 229;
            nextState.rechargeTimer = 5;
            simulate(nextState, false);
        }
    }

    auto initialState = GameState(50, 500, bossHP, bossDamage, 0, 0, 0, 0);
    simulate(initialState, true);
    writeln(minMana);
}
