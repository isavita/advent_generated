
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string input = File.ReadAllText("input.txt");
        string[] parts = input.Split('-');
        int start = int.Parse(parts[0]);
        int end = int.Parse(parts[1]);

        int count = 0;
        for (int i = start; i <= end; i++)
        {
            string s = i.ToString();
            if (HasDoubleAndIncreasingDigits(s))
            {
                count++;
            }
        }

        Console.WriteLine(count);
    }

    static bool HasDoubleAndIncreasingDigits(string s)
    {
        bool hasDouble = false;
        for (int i = 0; i < s.Length - 1; i++)
        {
            if (s[i] == s[i + 1])
            {
                hasDouble = true;
            }
            if (s[i] > s[i + 1])
            {
                return false;
            }
        }
        return hasDouble;
    }
}
