
using System;
using System.IO;
using System.Linq;

class Marble
{
    public int Value { get; set; }
    public Marble Prev { get; set; }
    public Marble Next { get; set; }
}

class Program
{
    static void Main()
    {
        var input = File.ReadAllLines("input.txt")[0].Split(' ');
        int players = int.Parse(input[0]);
        int lastMarble = int.Parse(input[6]);

        Console.WriteLine(PlayMarbleGame(players, lastMarble));
    }

    static int PlayMarbleGame(int players, int lastMarble)
    {
        int[] scores = new int[players];
        Marble current = new Marble { Value = 0 };
        current.Prev = current;
        current.Next = current;

        for (int marble = 1; marble <= lastMarble; marble++)
        {
            if (marble % 23 == 0)
            {
                int player = marble % players;
                for (int i = 0; i < 7; i++)
                {
                    current = current.Prev;
                }
                scores[player] += marble + current.Value;
                current.Prev.Next = current.Next;
                current.Next.Prev = current.Prev;
                current = current.Next;
            }
            else
            {
                current = current.Next;
                Marble newMarble = new Marble { Value = marble, Prev = current, Next = current.Next };
                current.Next.Prev = newMarble;
                current.Next = newMarble;
                current = newMarble;
            }
        }

        return scores.Max();
    }
}
