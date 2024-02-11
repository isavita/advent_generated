
using System;
using System.IO;
using System.Linq;

class Program
{
    static bool IsValidPassword(int password)
    {
        string s = password.ToString();
        bool hasDouble = false;

        for (int i = 0; i < s.Length - 1; i++)
        {
            if (s[i] > s[i + 1])
            {
                return false;
            }
            if (s[i] == s[i + 1])
            {
                if ((i == 0 || s[i] != s[i - 1]) && (i + 2 >= s.Length || s[i] != s[i + 2]))
                {
                    hasDouble = true;
                }
            }
        }

        return hasDouble;
    }

    static void Main()
    {
        string rangeStr = File.ReadAllText("input.txt").Trim();
        string[] ranges = rangeStr.Split('-');
        int start = int.Parse(ranges[0]);
        int end = int.Parse(ranges[1]);

        int count = 0;
        for (int i = start; i <= end; i++)
        {
            if (IsValidPassword(i))
            {
                count++;
            }
        }

        Console.WriteLine(count);
    }
}
