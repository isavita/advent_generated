
using System;
using System.IO;
using System.Text.RegularExpressions;
using System.Linq;
using System.Collections.Generic;

class Hand
{
    public string cards;
    public int bid;
}

class Program
{
    static void Main()
    {
        string input = File.ReadAllText("input.txt");
        string[] lines = input.Split('\n');

        List<Hand> hands = new List<Hand>();

        Regex re = new Regex(@"[\dAKQJT]+");
        Regex bidRe = new Regex(@" [\d]+");

        foreach (string line in lines)
        {
            if (line.Length == 0)
                continue;

            string cards = re.Match(line).Value;
            int bid = int.Parse(bidRe.Match(line).Value.Substring(1));

            hands.Add(new Hand { cards = cards, bid = bid });
        }

        List<Hand>[] matches = new List<Hand>[7];
        for (int i = 0; i < 7; i++)
        {
            matches[i] = new List<Hand>();
        }

        Dictionary<char, int> valueDict = new Dictionary<char, int> { { 'J', 1 }, { '2', 2 }, { '3', 3 }, { '4', 4 }, { '5', 5 }, { '6', 6 }, { '7', 7 }, { '8', 8 }, { '9', 9 }, { 'T', 10 }, { 'Q', 11 }, { 'K', 12 }, { 'A', 13 } };

        foreach (Hand hand in hands)
        {
            Dictionary<char, int> count = new Dictionary<char, int>();

            foreach (char c in hand.cards)
            {
                if (count.ContainsKey(c))
                    count[c]++;
                else
                    count[c] = 1;
            }

            if (count.ContainsKey('J') && count['J'] > 0)
            {
                int highV = 0;
                char highKey = 'J';
                foreach (char y in count.Keys)
                {
                    if (y != 'J')
                    {
                        if (count[y] > highV)
                        {
                            highKey = y;
                            highV = count[y];
                        }
                        else if (count[y] == highV && valueDict[y] > valueDict[highKey])
                        {
                            highKey = y;
                        }
                    }
                }
                if (highKey != 'J')
                {
                    count[highKey] += count['J'];
                    count.Remove('J');
                }
            }

            int value = 1;
            foreach (int i in count.Values)
            {
                value *= i;
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
                        matches[1].Add(hand);
                    else
                        matches[4].Add(hand);
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

        List<int[]> convertedMatches = new List<int[]>();

        foreach (List<Hand> x in matches)
        {
            List<int[]> temp = new List<int[]>();
            foreach (Hand i in x)
            {
                string y = i.cards.Replace("A", "E").Replace("T", "A").Replace("J", "1").Replace("Q", "C").Replace("K", "D");
                int val = Convert.ToInt32(y, 16);
                temp.Add(new int[] { val, i.bid });
            }
            temp = temp.OrderByDescending(i => i[0]).ToList();
            convertedMatches.AddRange(temp);
        }

        int total = 0;
        for (int x = 0; x < convertedMatches.Count; x++)
        {
            total += convertedMatches[x][1] * (convertedMatches.Count - x);
        }

        Console.WriteLine(total);
    }
}
