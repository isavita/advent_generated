
using System;
using System.IO;
using System.Linq;
using System.Collections.Generic;

class Program
{
    static string SortString(string word)
    {
        var sortedChars = word.ToCharArray();
        Array.Sort(sortedChars);
        return new string(sortedChars);
    }

    static void Main()
    {
        string[] passphrases = File.ReadAllLines("input.txt");
        int validCount = 0;

        foreach (var passphrase in passphrases)
        {
            var words = passphrase.Split(' ');
            var wordSet = new HashSet<string>();

            bool valid = true;
            foreach (var word in words)
            {
                var sortedWord = SortString(word);
                if (wordSet.Contains(sortedWord))
                {
                    valid = false;
                    break;
                }
                wordSet.Add(sortedWord);
            }

            if (valid)
            {
                validCount++;
            }
        }

        Console.WriteLine(validCount);
    }
}
