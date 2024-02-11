
using System;
using System.IO;
using System.Linq;

class Program
{
    const int Size = 10007;

    static void Main()
    {
        int[] deck = Enumerable.Range(0, Size).ToArray();

        string[] lines = File.ReadAllLines("input.txt");

        foreach (string line in lines)
        {
            if (line == "deal into new stack")
            {
                deck = DealIntoNewStack(deck);
            }
            else if (line.StartsWith("cut"))
            {
                int n = int.Parse(line.Split(' ')[1]);
                deck = CutN(deck, n);
            }
            else if (line.StartsWith("deal with increment"))
            {
                int n = int.Parse(line.Split(' ').Last());
                deck = DealWithIncrement(deck, n);
            }
        }

        Console.WriteLine(Find2019(deck));
    }

    static int[] DealIntoNewStack(int[] deck)
    {
        for (int i = 0; i < Size / 2; i++)
        {
            int temp = deck[i];
            deck[i] = deck[Size - i - 1];
            deck[Size - i - 1] = temp;
        }
        return deck;
    }

    static int[] CutN(int[] deck, int n)
    {
        if (n >= 0)
        {
            return deck.Skip(n).Concat(deck.Take(n)).ToArray();
        }
        else
        {
            return deck.Skip(deck.Length + n).Concat(deck.Take(deck.Length + n)).ToArray();
        }
    }

    static int[] DealWithIncrement(int[] deck, int n)
    {
        int[] newDeck = new int[Size];

        for (int i = 0; i < Size; i++)
        {
            newDeck[(i * n) % Size] = deck[i];
        }

        return newDeck;
    }

    static int Find2019(int[] deck)
    {
        for (int i = 0; i < Size; i++)
        {
            if (deck[i] == 2019)
            {
                return i;
            }
        }
        return -1;
    }
}
