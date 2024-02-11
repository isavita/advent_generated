
using System;
using System.IO;
using System.Linq;

class Program
{
    static void Main()
    {
        string[] passphrases = File.ReadAllLines("input.txt");
        int validCount = 0;

        foreach (var passphrase in passphrases)
        {
            string[] words = passphrase.Split(' ');
            var wordSet = words.Distinct().ToHashSet();

            if (words.Length == wordSet.Count)
            {
                validCount++;
            }
        }

        Console.WriteLine(validCount);
    }
}
