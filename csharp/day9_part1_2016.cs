
using System;
using System.IO;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string input = File.ReadAllText("input.txt");
        int decompressedLength = GetDecompressedLength(input);
        Console.WriteLine(decompressedLength);
    }

    static int GetDecompressedLength(string input)
    {
        Regex markerRegex = new Regex(@"\((\d+)x(\d+)\)");
        int length = 0;
        for (int i = 0; i < input.Length;)
        {
            Match marker = markerRegex.Match(input, i);
            if (marker.Success)
            {
                int charCount = int.Parse(marker.Groups[1].Value);
                int repeatCount = int.Parse(marker.Groups[2].Value);
                length += charCount * repeatCount;
                i = marker.Index + marker.Length + charCount;
            }
            else
            {
                length++;
                i++;
            }
        }
        return length;
    }
}
