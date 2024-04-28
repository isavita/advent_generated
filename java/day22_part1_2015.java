import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class Main {

    static class GameState {
        int playerHP, playerMana, bossHP, bossDamage;
        int shieldTimer, poisonTimer, rechargeTimer;
        int manaSpent;
    }

    static int minMana = Integer.MAX_VALUE;

    static int minManaToWin(GameState state, boolean playerTurn) {
        if (state.manaSpent >= minMana) {
            return minMana;
        }
        if (state.bossHP <= 0) {
            minMana = Math.min(minMana, state.manaSpent);
            return minMana;
        }
        if (state.playerHP <= 0) {
            return Integer.MAX_VALUE;
        }

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
            minManaToWin(state, true);
            return minMana;
        }

        if (state.playerMana >= 53) {
            GameState newState = new GameState();
            newState.playerHP = state.playerHP;
            newState.playerMana = state.playerMana - 53;
            newState.bossHP = state.bossHP - 4;
            newState.bossDamage = state.bossDamage;
            newState.manaSpent = state.manaSpent + 53;
            newState.shieldTimer = state.shieldTimer;
            newState.poisonTimer = state.poisonTimer;
            newState.rechargeTimer = state.rechargeTimer;
            minManaToWin(newState, false);
        }
        if (state.playerMana >= 73) {
            GameState newState = new GameState();
            newState.playerHP = state.playerHP + 2;
            newState.playerMana = state.playerMana - 73;
            newState.bossHP = state.bossHP - 2;
            newState.bossDamage = state.bossDamage;
            newState.manaSpent = state.manaSpent + 73;
            newState.shieldTimer = state.shieldTimer;
            newState.poisonTimer = state.poisonTimer;
            newState.rechargeTimer = state.rechargeTimer;
            minManaToWin(newState, false);
        }
        if (state.playerMana >= 113 && state.shieldTimer == 0) {
            GameState newState = new GameState();
            newState.playerHP = state.playerHP;
            newState.playerMana = state.playerMana - 113;
            newState.bossHP = state.bossHP;
            newState.bossDamage = state.bossDamage;
            newState.manaSpent = state.manaSpent + 113;
            newState.shieldTimer = 6;
            newState.poisonTimer = state.poisonTimer;
            newState.rechargeTimer = state.rechargeTimer;
            minManaToWin(newState, false);
        }
        if (state.playerMana >= 173 && state.poisonTimer == 0) {
            GameState newState = new GameState();
            newState.playerHP = state.playerHP;
            newState.playerMana = state.playerMana - 173;
            newState.bossHP = state.bossHP;
            newState.bossDamage = state.bossDamage;
            newState.manaSpent = state.manaSpent + 173;
            newState.shieldTimer = state.shieldTimer;
            newState.poisonTimer = 6;
            newState.rechargeTimer = state.rechargeTimer;
            minManaToWin(newState, false);
        }
        if (state.playerMana >= 229 && state.rechargeTimer == 0) {
            GameState newState = new GameState();
            newState.playerHP = state.playerHP;
            newState.playerMana = state.playerMana - 229;
            newState.bossHP = state.bossHP;
            newState.bossDamage = state.bossDamage;
            newState.manaSpent = state.manaSpent + 229;
            newState.shieldTimer = state.shieldTimer;
            newState.poisonTimer = state.poisonTimer;
            newState.rechargeTimer = 5;
            minManaToWin(newState, false);
        }
        return minMana;
    }

    public static void main(String[] args) throws IOException {
        BufferedReader br = new BufferedReader(new FileReader("input.txt"));
        String line = br.readLine();
        int bossHP = Integer.parseInt(line.split(": ")[1]);
        line = br.readLine();
        int bossDamage = Integer.parseInt(line.split(": ")[1]);

        GameState initialState = new GameState();
        initialState.playerHP = 50;
        initialState.playerMana = 500;
        initialState.bossHP = bossHP;
        initialState.bossDamage = bossDamage;
        minMana = Integer.MAX_VALUE;
        minManaToWin(initialState, true);
        System.out.println(minMana);
    }
}