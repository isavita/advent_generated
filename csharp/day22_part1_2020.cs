
using System;
using System.IO;
using System.Linq;

class Program
{
    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");
        int[] player1Deck = Array.ConvertAll(lines.Skip(1).TakeWhile(line => !string.IsNullOrEmpty(line)).ToArray(), int.Parse);
        int[] player2Deck = Array.ConvertAll(lines.SkipWhile(line => line != "").Skip(2).ToArray(), int.Parse);

        while (player1Deck.Length > 0 && player2Deck.Length > 0)
        {
            int card1 = player1Deck[0];
            int card2 = player2Deck[0];
            player1Deck = player1Deck.Skip(1).ToArray();
            player2Deck = player2Deck.Skip(1).ToArray();

            if (card1 > card2)
            {
                player1Deck = player1Deck.Concat(new int[] { card1, card2 }).ToArray();
            }
            else
            {
                player2Deck = player2Deck.Concat(new int[] { card2, card1 }).ToArray();
            }
        }

        int[] winningDeck = player1Deck.Length > 0 ? player1Deck : player2Deck;

        int score = winningDeck.Select((card, index) => card * (winningDeck.Length - index)).Sum();
        Console.WriteLine(score);
    }
}
