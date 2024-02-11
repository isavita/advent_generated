
using System;
using System.IO;
using System.Text.RegularExpressions;
using System.Collections.Generic;

class Card
{
    public Dictionary<string, int> winnings;
    public Dictionary<string, int> givens;
    public int totalCount;
}

class Program
{
    static int GetPointsForCard(Card card)
    {
        int points = 0;
        foreach (var kvp in card.givens)
        {
            if (card.winnings.ContainsKey(kvp.Key))
            {
                points += kvp.Value * card.winnings[kvp.Key];
            }
        }
        return points;
    }

    static Card LexLineIntoCard(string line)
    {
        string[] parts = line.Split(new string[] { ": " }, StringSplitOptions.None);
        string[] cardData = parts[1].Split(new string[] { " | " }, StringSplitOptions.None);

        Regex regex = new Regex("[0-9]{1,2}");

        var winnings = new Dictionary<string, int>();
        foreach (Match match in regex.Matches(cardData[0]))
        {
            string point = match.Value;
            if (winnings.ContainsKey(point))
            {
                winnings[point]++;
            }
            else
            {
                winnings.Add(point, 1);
            }
        }

        var givens = new Dictionary<string, int>();
        foreach (Match match in regex.Matches(cardData[1]))
        {
            string point = match.Value;
            if (givens.ContainsKey(point))
            {
                givens[point]++;
            }
            else
            {
                givens.Add(point, 1);
            }
        }

        return new Card
        {
            winnings = winnings,
            givens = givens,
            totalCount = 1
        };
    }

    static void Main()
    {
        string input = File.ReadAllText("input.txt").Trim();
        var cards = new List<Card>();

        foreach (string line in input.Split('\n'))
        {
            if (line.Length == 0)
            {
                continue;
            }
            Card card = LexLineIntoCard(line);
            cards.Add(card);
        }

        for (int i = 0; i < cards.Count; i++)
        {
            int points = GetPointsForCard(cards[i]);

            for (int j = 1; j <= points; j++)
            {
                if (i + j < cards.Count)
                {
                    cards[i + j].totalCount += 1 * cards[i].totalCount;
                }
            }
        }

        int totalCards = 0;
        foreach (var card in cards)
        {
            totalCards += card.totalCount;
        }

        Console.WriteLine(totalCards);
    }
}
