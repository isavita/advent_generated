
using System;
using System.IO;
using System.Text.RegularExpressions;
using System.Linq;

class Program
{
    static void Main()
    {
        int sslCount = 0;
        using (StreamReader sr = new StreamReader("input.txt"))
        {
            string line;
            while ((line = sr.ReadLine()) != null)
            {
                if (SupportsSSL(line))
                {
                    sslCount++;
                }
            }
        }

        Console.WriteLine(sslCount);
    }

    static bool SupportsSSL(string ip)
    {
        Regex insideBrackets = new Regex(@"\[[a-z]+\]");
        MatchCollection bracketContents = insideBrackets.Matches(ip);

        ip = insideBrackets.Replace(ip, "-");
        foreach (string aba in FindABAs(ip))
        {
            string bab = new string(new char[] { aba[1], aba[0], aba[1] });
            foreach (Match bracketContent in bracketContents)
            {
                if (bracketContent.Value.Contains(bab))
                {
                    return true;
                }
            }
        }

        return false;
    }

    static string[] FindABAs(string s)
    {
        return Enumerable.Range(0, s.Length - 2)
            .Where(i => s[i] != s[i + 1] && s[i] == s[i + 2])
            .Select(i => s.Substring(i, 3))
            .ToArray();
    }
}
