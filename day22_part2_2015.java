
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

class Solution {
    static class GameState {
        int playerHP, playerMana, bossHP, bossDamage;
        int shieldTimer, poisonTimer, rechargeTimer;
        int manaSpent;

        public GameState(int playerHP, int playerMana, int bossHP, int bossDamage, int shieldTimer, int poisonTimer, int rechargeTimer, int manaSpent) {
            this.playerHP = playerHP;
            this.playerMana = playerMana;
            this.bossHP = bossHP;
            this.bossDamage = bossDamage;
            this.shieldTimer = shieldTimer;
            this.poisonTimer = poisonTimer;
            this.rechargeTimer = rechargeTimer;
            this.manaSpent = manaSpent;
        }
    }

    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            int bossHP = Integer.parseInt(reader.readLine().split(": ")[1]);
            int bossDamage = Integer.parseInt(reader.readLine().split(": ")[1]);
            reader.close();

            GameState initialState = new GameState(50, 500, bossHP, bossDamage, 0, 0, 0, 0);
            System.out.println(minManaToWin(initialState));
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public static int minManaToWin(GameState initialState) {
        final int[] minMana = {Integer.MAX_VALUE};
        simulate(initialState, true, minMana);
        return minMana[0];
    }

    public static void simulate(GameState state, boolean playerTurn, int[] minMana) {
        if (state.manaSpent >= minMana[0]) {
            return;
        }
        if (state.bossHP <= 0) {
            minMana[0] = state.manaSpent;
            return;
        }
        if (state.playerHP <= 0) {
            return;
        }

        if (playerTurn) {
            state.playerHP--;
            if (state.playerHP <= 0) {
                return;
            }
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
            simulate(state, true, minMana);
            return;
        }

        if (state.playerMana >= 53) {
            GameState newState = new GameState(state.playerHP, state.playerMana - 53, state.bossHP - 4, state.bossDamage, state.shieldTimer, state.poisonTimer, state.rechargeTimer, state.manaSpent + 53);
            simulate(newState, false, minMana);
        }
        if (state.playerMana >= 73) {
            GameState newState = new GameState(state.playerHP + 2, state.playerMana - 73, state.bossHP - 2, state.bossDamage, state.shieldTimer, state.poisonTimer, state.rechargeTimer, state.manaSpent + 73);
            simulate(newState, false, minMana);
        }
        if (state.playerMana >= 113 && state.shieldTimer == 0) {
            GameState newState = new GameState(state.playerHP, state.playerMana - 113, state.bossHP, state.bossDamage, 6, state.poisonTimer, state.rechargeTimer, state.manaSpent + 113);
            simulate(newState, false, minMana);
        }
        if (state.playerMana >= 173 && state.poisonTimer == 0) {
            GameState newState = new GameState(state.playerHP, state.playerMana - 173, state.bossHP, state.bossDamage, state.shieldTimer, 6, state.rechargeTimer, state.manaSpent + 173);
            simulate(newState, false, minMana);
        }
        if (state.playerMana >= 229 && state.rechargeTimer == 0) {
            GameState newState = new GameState(state.playerHP, state.playerMana - 229, state.bossHP, state.bossDamage, state.shieldTimer, state.poisonTimer, 5, state.manaSpent + 229);
            simulate(newState, false, minMana);
        }
    }
}
