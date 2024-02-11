
using System;
using System.IO;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        int tlsCount = 0;
        using (StreamReader sr = new StreamReader("input.txt"))
        {
            string line;
            while ((line = sr.ReadLine()) != null)
            {
                if (SupportsTLS(line))
                {
                    tlsCount++;
                }
            }
        }

        Console.WriteLine(tlsCount);
    }

    static bool SupportsTLS(string ip)
    {
        Regex insideBrackets = new Regex(@"\[[a-z]+\]");
        MatchCollection bracketContents = insideBrackets.Matches(ip);

        foreach (Match bracketContent in bracketContents)
        {
            if (ContainsABBA(bracketContent.Value))
            {
                return false;
            }
        }

        ip = insideBrackets.Replace(ip, "-");
        return ContainsABBA(ip);
    }

    static bool ContainsABBA(string s)
    {
        for (int i = 0; i < s.Length - 3; i++)
        {
            if (s[i] != s[i + 1] && s[i] == s[i + 3] && s[i + 1] == s[i + 2])
            {
                return true;
            }
        }
        return false;
    }
}
