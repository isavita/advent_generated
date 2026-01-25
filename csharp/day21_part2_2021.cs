
using System;
using System.Collections.Generic;
using System.IO;

public struct GameState
{
    public int[] Positions;
    public int[] Scores;
    public int RollsLeftInTurn;
    public bool IsPlayer1sTurn;

    public override bool Equals(object obj)
    {
        if (obj is GameState other)
        {
            return Positions[0] == other.Positions[0] &&
                   Positions[1] == other.Positions[1] &&
                   Scores[0] == other.Scores[0] &&
                   Scores[1] == other.Scores[1] &&
                   RollsLeftInTurn == other.RollsLeftInTurn &&
                   IsPlayer1sTurn == other.IsPlayer1sTurn;
        }
        return false;
    }

    public override int GetHashCode()
    {
        unchecked
        {
            int hash = 17;
            hash = hash * 23 + Positions[0].GetHashCode();
            hash = hash * 23 + Positions[1].GetHashCode();
            hash = hash * 23 + Scores[0].GetHashCode();
            hash = hash * 23 + Scores[1].GetHashCode();
            hash = hash * 23 + RollsLeftInTurn.GetHashCode();
            hash = hash * 23 + IsPlayer1sTurn.GetHashCode();
            return hash;
        }
    }
}

public struct WinCounts
{
    public long Wins1;
    public long Wins2;
}

class Program
{
    private static Dictionary<GameState, WinCounts> cache = new Dictionary<GameState, WinCounts>();

    static WinCounts Play(GameState state)
    {
        if (cache.TryGetValue(state, out WinCounts wins))
        {
            return wins;
        }

        int playerIndex = state.IsPlayer1sTurn ? 0 : 1;

        if (state.RollsLeftInTurn == 0)
        {
            state.Scores[playerIndex] += state.Positions[playerIndex];
            if (state.Scores[playerIndex] >= 21)
            {
                wins = new WinCounts { Wins1 = playerIndex == 0 ? 1 : 0, Wins2 = playerIndex == 1 ? 1 : 0 };
                cache[state] = wins;
                return wins;
            }
            state.IsPlayer1sTurn = !state.IsPlayer1sTurn;
            state.RollsLeftInTurn = 3;
            playerIndex = (playerIndex + 1) % 2;
        }

        wins = new WinCounts();
        for (int roll = 1; roll <= 3; roll++)
        {
            GameState nextState = state;
            nextState.Positions = (int[])state.Positions.Clone();
            nextState.Scores = (int[])state.Scores.Clone();
            nextState.Positions[playerIndex] = (nextState.Positions[playerIndex] + roll - 1) % 10 + 1;
            nextState.RollsLeftInTurn--;
            WinCounts nextWins = Play(nextState);
            wins.Wins1 += nextWins.Wins1;
            wins.Wins2 += nextWins.Wins2;
        }
        cache[state] = wins;
        return wins;
    }

    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");
        int[] positions = new int[2];
        positions[0] = int.Parse(lines[0].Split(": ")[1]);
        positions[1] = int.Parse(lines[1].Split(": ")[1]);

        GameState initialState = new GameState
        {
            Positions = positions,
            Scores = new int[] { 0, 0 },
            RollsLeftInTurn = 3,
            IsPlayer1sTurn = true
        };

        WinCounts finalWins = Play(initialState);
        Console.WriteLine(Math.Max(finalWins.Wins1, finalWins.Wins2));
    }
}
