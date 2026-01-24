using System;
using System.IO;

class Program
{
    static long Decompress(string s, int start, int end)
    {
        long length = 0;
        int i = start;
        while (i < end)
        {
            if (s[i] == '(')
            {
                int j = i + 1;
                int charCount = 0;
                while (j < end && char.IsDigit(s[j]))
                    charCount = charCount * 10 + (s[j++] - '0');
                j++; // skip 'x' or separator
                int repeatCount = 0;
                while (j < end && char.IsDigit(s[j]))
                    repeatCount = repeatCount * 10 + (s[j++] - '0');
                int next = j + 1; // skip ')'
                length += repeatCount * Decompress(s, next, next + charCount);
                i = next + charCount;
            }
            else
            {
                length++;
                i++;
            }
        }
        return length;
    }

    static void Main()
    {
        string input = File.ReadAllText("input.txt");
        long result = Decompress(input, 0, input.Length);
        Console.WriteLine(result);
    }
}