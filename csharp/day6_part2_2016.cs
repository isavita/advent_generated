using System;
using System.IO;

class Program
{
    static void Main()
    {
        var lines = File.ReadAllLines("input.txt");
        if (lines.Length == 0) { Console.WriteLine(); return; }

        int length = lines[0].Length;
        char[] result = new char[length];

        for (int col = 0; col < length; col++)
        {
            int[] freq = new int[256];
            foreach (var line in lines) freq[(int)line[col]]++;

            int minCount = int.MaxValue;
            char minChar = '\0';
            for (int i = 0; i < 256; i++)
                if (freq[i] > 0 && freq[i] < minCount)
                {
                    minCount = freq[i];
                    minChar = (char)i;
                }

            result[col] = minChar;
        }

        Console.WriteLine(new string(result));
    }
}