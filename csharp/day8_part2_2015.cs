
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");
        int totalDiff = 0;

        foreach (string line in lines)
        {
            int originalLength = line.Length;
            int encodedLength = CalculateEncodedLength(line);
            totalDiff += encodedLength - originalLength;
        }

        Console.WriteLine(totalDiff);
    }

    static int CalculateEncodedLength(string s)
    {
        string encoded = "\"";
        foreach (char ch in s)
        {
            if (ch == '\\' || ch == '"')
            {
                encoded += "\\";
            }
            encoded += ch;
        }
        encoded += "\"";
        return encoded.Length;
    }
}
