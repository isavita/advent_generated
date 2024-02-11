
using System;
using System.IO;
using System.Text;

class Program
{
    static void Main()
    {
        string initialSequence = File.ReadAllText("input.txt").Trim();
        string result = LookAndSay(initialSequence, 40);
        Console.WriteLine(result.Length);
    }

    static string LookAndSay(string sequence, int iterations)
    {
        for (int i = 0; i < iterations; i++)
        {
            sequence = NextSequence(sequence);
        }
        return sequence;
    }

    static string NextSequence(string sequence)
    {
        StringBuilder result = new StringBuilder();
        for (int i = 0; i < sequence.Length;)
        {
            int count = 1;
            char digit = sequence[i];
            for (int j = i + 1; j < sequence.Length && sequence[j] == digit; j++)
            {
                count++;
            }
            result.Append(count).Append(digit);
            i += count;
        }
        return result.ToString();
    }
}
