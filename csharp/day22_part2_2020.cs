
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

public class Deck
{
    public List<int> Cards { get; private set; }

    public Deck()
    {
        Cards = new List<int>();
    }

    public void PushBack(int card)
    {
        Cards.Add(card);
    }

    public int PopFront()
    {
        int card = Cards[0];
        Cards.RemoveAt(0);
        return card;
    }

    public Deck Copy(int n)
    {
        Deck copy = new Deck();
        copy.Cards = Cards.Take(n).ToList();
        return copy;
    }

    public long CalculateScore()
    {
        return Cards.Select((card, index) => (long)card * (Cards.Count - index)).Sum();
    }
}

public class HashSet
{
    private Dictionary<string, bool> set;

    public HashSet()
    {
        set = new Dictionary<string, bool>();
    }

    public bool Add(string stateKey)
    {
        if (set.ContainsKey(stateKey))
        {
            return true;
        }
        set[stateKey] = true;
        return false;
    }
}

public class Program
{
    private static bool PlayRecursiveCombat(Deck p1, Deck p2)
    {
        HashSet previousRounds = new HashSet();

        while (p1.Cards.Count > 0 && p2.Cards.Count > 0)
        {
            string roundKey = string.Join(",", p1.Cards) + "|" + string.Join(",", p2.Cards);
            if (previousRounds.Add(roundKey))
            {
                return true; // Player 1 wins due to repeated state
            }

            int card1 = p1.PopFront();
            int card2 = p2.PopFront();

            bool p1WinsRound;
            if (p1.Cards.Count >= card1 && p2.Cards.Count >= card2)
            {
                Deck subP1 = p1.Copy(card1);
                Deck subP2 = p2.Copy(card2);
                p1WinsRound = PlayRecursiveCombat(subP1, subP2);
            }
            else
            {
                p1WinsRound = card1 > card2;
            }

            if (p1WinsRound)
            {
                p1.PushBack(card1);
                p1.PushBack(card2);
            }
            else
            {
                p2.PushBack(card2);
                p2.PushBack(card1);
            }
        }

        return p1.Cards.Count > 0;
    }

    public static void Main(string[] args)
    {
        string[] lines = File.ReadAllLines("input.txt");

        Deck player1Deck = new Deck();
        Deck player2Deck = new Deck();
        Deck currentDeck = player1Deck;

        bool readingPlayer1 = true;

        foreach (string line in lines)
        {
            if (string.IsNullOrEmpty(line))
            {
                if (readingPlayer1)
                {
                    currentDeck = player2Deck;
                    readingPlayer1 = false;
                }
                continue;
            }

            if (line.StartsWith("Player"))
            {
                continue;
            }

            if (int.TryParse(line, out int card))
            {
                currentDeck.PushBack(card);
            }
        }

        bool p1WonGame = PlayRecursiveCombat(player1Deck, player2Deck);

        Deck winningDeck = p1WonGame ? player1Deck : player2Deck;
        Console.WriteLine(winningDeck.CalculateScore());
    }
}
