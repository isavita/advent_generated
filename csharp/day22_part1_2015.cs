
using System;
using System.IO;

class Program
{
    struct GameState
    {
        public int playerHP, playerMana, bossHP, bossDamage;
        public int shieldTimer, poisonTimer, rechargeTimer;
        public int manaSpent;
    }

    static int MinManaToWin(GameState initialState)
    {
        int minMana = int.MaxValue;
        Action<GameState, bool> simulate = null;
        simulate = (state, playerTurn) =>
        {
            if (state.manaSpent >= minMana)
                return;
            if (state.bossHP <= 0)
            {
                minMana = state.manaSpent;
                return;
            }
            if (state.playerHP <= 0)
                return;

            if (state.shieldTimer > 0)
                state.shieldTimer--;
            if (state.poisonTimer > 0)
            {
                state.bossHP -= 3;
                state.poisonTimer--;
            }
            if (state.rechargeTimer > 0)
            {
                state.playerMana += 101;
                state.rechargeTimer--;
            }

            if (!playerTurn)
            {
                int damage = state.bossDamage;
                if (state.shieldTimer > 0)
                    damage -= 7;
                if (damage < 1)
                    damage = 1;
                state.playerHP -= damage;
                simulate(state, true);
                return;
            }

            if (state.playerMana >= 53)
            {
                var newState = state;
                newState.playerMana -= 53;
                newState.manaSpent += 53;
                newState.bossHP -= 4;
                simulate(newState, false);
            }
            if (state.playerMana >= 73)
            {
                var newState = state;
                newState.playerMana -= 73;
                newState.manaSpent += 73;
                newState.bossHP -= 2;
                newState.playerHP += 2;
                simulate(newState, false);
            }
            if (state.playerMana >= 113 && state.shieldTimer == 0)
            {
                var newState = state;
                newState.playerMana -= 113;
                newState.manaSpent += 113;
                newState.shieldTimer = 6;
                simulate(newState, false);
            }
            if (state.playerMana >= 173 && state.poisonTimer == 0)
            {
                var newState = state;
                newState.playerMana -= 173;
                newState.manaSpent += 173;
                newState.poisonTimer = 6;
                simulate(newState, false);
            }
            if (state.playerMana >= 229 && state.rechargeTimer == 0)
            {
                var newState = state;
                newState.playerMana -= 229;
                newState.manaSpent += 229;
                newState.rechargeTimer = 5;
                simulate(newState, false);
            }
        };

        initialState.playerHP = 50;
        initialState.playerMana = 500;
        simulate(initialState, true);
        return minMana;
    }

    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");
        int bossHP = int.Parse(lines[0].Split(": ")[1]);
        int bossDamage = int.Parse(lines[1].Split(": ")[1]);

        GameState initialState = new GameState { bossHP = bossHP, bossDamage = bossDamage };
        Console.WriteLine(MinManaToWin(initialState));
    }
}
