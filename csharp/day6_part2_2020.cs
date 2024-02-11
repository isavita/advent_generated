
using System;
using System.Collections.Generic;
using System.IO;

class Program
{
    static void Main()
    {
        StreamReader file = new StreamReader("input.txt");
        int totalCount = 0;
        Dictionary<char, int> groupAnswers = new Dictionary<char, int>();
        int groupSize = 0;

        string line;
        while ((line = file.ReadLine()) != null)
        {
            if (line == "")
            {
                foreach (var count in groupAnswers.Values)
                {
                    if (count == groupSize)
                    {
                        totalCount++;
                    }
                }
                groupAnswers.Clear();
                groupSize = 0;
            }
            else
            {
                groupSize++;
                foreach (char question in line)
                {
                    if (groupAnswers.ContainsKey(question))
                    {
                        groupAnswers[question]++;
                    }
                    else
                    {
                        groupAnswers[question] = 1;
                    }
                }
            }
        }

        foreach (var count in groupAnswers.Values)
        {
            if (count == groupSize)
            {
                totalCount++;
            }
        }

        Console.WriteLine(totalCount);
    }
}
