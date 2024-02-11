
using System;
using System.IO;
using System.Text.RegularExpressions;
using System.Linq;
using System.Collections.Generic;

public class Hand
{
    public string cards;
    public int bid;
}

public class RankedHand
{
    public Hand hand;
    public int rank;
}

public class Solution
{
    const int HighCard = 1;
    const int OnePair = 2;
    const int TwoPair = 3;
    const int ThreeKind = 4;
    const int FullHouse = 5;
    const int FourKind = 6;
    const int FiveKind = 7;

    static List<Hand>[] matches = new List<Hand>[] {
        new List<Hand>(), new List<Hand>(), new List<Hand>(), new List<Hand>(), new List<Hand>(), new List<Hand>(), new List<Hand>()
    };

    static void FindMatches(List<Hand> hands)
    {
        foreach (var hand in hands)
        {
            var count = new Dictionary<char, int>();

            foreach (var card in hand.cards)
            {
                if (count.ContainsKey(card))
                {
                    count[card]++;
                }
                else
                {
                    count[card] = 1;
                }
            }

            int value = 1;
            foreach (var c in count.Values)
            {
                value *= c;
            }

            switch (value)
            {
                case 1:
                    matches[6].Add(hand);
                    break;
                case 2:
                    matches[5].Add(hand);
                    break;
                case 3:
                    matches[3].Add(hand);
                    break;
                case 4:
                    if (count.Count == 2)
                    {
                        matches[1].Add(hand);
                    }
                    else
                    {
                        matches[4].Add(hand);
                    }
                    break;
                case 5:
                    matches[0].Add(hand);
                    break;
                case 6:
                    matches[2].Add(hand);
                    break;
                default:
                    Console.WriteLine("oh no");
                    break;
            }
        }
    }

    static List<RankedHand> ConvertAndOrderMatches()
    {
        List<RankedHand> convertedMatches = new List<RankedHand>();

        foreach (var category in matches)
        {
            List<RankedHand> temp = new List<RankedHand>();

            foreach (var hand in category)
            {
                string cards = Regex.Replace(hand.cards, "A", "E");
                cards = Regex.Replace(cards, "T", "A");
                cards = Regex.Replace(cards, "J", "B");
                cards = Regex.Replace(cards, "Q", "C");
                cards = Regex.Replace(cards, "K", "D");

                int num = Convert.ToInt32(cards, 16);

                temp.Add(new RankedHand { hand = hand, rank = num });
            }

            temp = temp.OrderByDescending(x => x.rank).ToList();

            convertedMatches.AddRange(temp);
        }

        return convertedMatches;
    }

    public static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");
        List<Hand> hands = new List<Hand>();

        Regex re = new Regex(@"[\dAKQJT]+");
        Regex bidRe = new Regex(@" [\d]+");

        foreach (var line in lines)
        {
            if (line.Length == 0)
            {
                continue;
            }

            string cards = re.Match(line).Value;
            int bid = int.Parse(bidRe.Match(line).Value.Substring(1));

            hands.Add(new Hand { cards = cards, bid = bid });
        }

        FindMatches(hands);

        List<RankedHand> convertedMatches = ConvertAndOrderMatches();

        int total = 0;
        for (int i = 0; i < convertedMatches.Count; i++)
        {
            total += convertedMatches[i].hand.bid * (convertedMatches.Count - i);
        }

        Console.WriteLine(total);
    }
}
