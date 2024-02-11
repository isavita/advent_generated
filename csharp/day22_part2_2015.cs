
using System;
using System.IO;

class Program
{
    class GameState
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

            if (playerTurn)
            {
                state.playerHP--;
                if (state.playerHP <= 0)
                    return;
            }

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
                var newState = new GameState
                {
                    playerHP = state.playerHP,
                    playerMana = state.playerMana - 53,
                    bossHP = state.bossHP - 4,
                    bossDamage = state.bossDamage,
                    shieldTimer = state.shieldTimer,
                    poisonTimer = state.poisonTimer,
                    rechargeTimer = state.rechargeTimer,
                    manaSpent = state.manaSpent + 53
                };
                simulate(newState, false);
            }
            if (state.playerMana >= 73)
            {
                var newState = new GameState
                {
                    playerHP = state.playerHP + 2,
                    playerMana = state.playerMana - 73,
                    bossHP = state.bossHP - 2,
                    bossDamage = state.bossDamage,
                    shieldTimer = state.shieldTimer,
                    poisonTimer = state.poisonTimer,
                    rechargeTimer = state.rechargeTimer,
                    manaSpent = state.manaSpent + 73
                };
                simulate(newState, false);
            }
            if (state.playerMana >= 113 && state.shieldTimer == 0)
            {
                var newState = new GameState
                {
                    playerHP = state.playerHP,
                    playerMana = state.playerMana - 113,
                    bossHP = state.bossHP,
                    bossDamage = state.bossDamage,
                    shieldTimer = 6,
                    poisonTimer = state.poisonTimer,
                    rechargeTimer = state.rechargeTimer,
                    manaSpent = state.manaSpent + 113
                };
                simulate(newState, false);
            }
            if (state.playerMana >= 173 && state.poisonTimer == 0)
            {
                var newState = new GameState
                {
                    playerHP = state.playerHP,
                    playerMana = state.playerMana - 173,
                    bossHP = state.bossHP,
                    bossDamage = state.bossDamage,
                    shieldTimer = state.shieldTimer,
                    poisonTimer = 6,
                    rechargeTimer = state.rechargeTimer,
                    manaSpent = state.manaSpent + 173
                };
                simulate(newState, false);
            }
            if (state.playerMana >= 229 && state.rechargeTimer == 0)
            {
                var newState = new GameState
                {
                    playerHP = state.playerHP,
                    playerMana = state.playerMana - 229,
                    bossHP = state.bossHP,
                    bossDamage = state.bossDamage,
                    shieldTimer = state.shieldTimer,
                    poisonTimer = state.poisonTimer,
                    rechargeTimer = 5,
                    manaSpent = state.manaSpent + 229
                };
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
