
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

class Program
{
    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");
        int totalCount = 0;
        var groupAnswers = new HashSet<char>();

        foreach (var line in lines)
        {
            if (string.IsNullOrEmpty(line))
            {
                totalCount += groupAnswers.Count;
                groupAnswers.Clear();
            }
            else
            {
                foreach (char question in line)
                {
                    groupAnswers.Add(question);
                }
            }
        }

        totalCount += groupAnswers.Count;
        Console.WriteLine(totalCount);
    }
}
