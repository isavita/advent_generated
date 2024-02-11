
using System;
using System.IO;
using System.Collections.Generic;

class Program
{
    static int ItemPriority(char item)
    {
        if (item >= 'a' && item <= 'z')
        {
            return item - 'a' + 1;
        }
        return item - 'A' + 27;
    }

    static void Main()
    {
        int sum = 0;
        using (StreamReader sr = new StreamReader("input.txt"))
        {
            string line;
            while ((line = sr.ReadLine()) != null)
            {
                int half = line.Length / 2;
                string firstCompartment = line.Substring(0, half);
                string secondCompartment = line.Substring(half);

                Dictionary<char, int> compartmentMap = new Dictionary<char, int>();
                foreach (char item in firstCompartment)
                {
                    if (compartmentMap.ContainsKey(item))
                    {
                        compartmentMap[item]++;
                    }
                    else
                    {
                        compartmentMap[item] = 1;
                    }
                }

                foreach (char item in secondCompartment)
                {
                    if (compartmentMap.ContainsKey(item))
                    {
                        sum += ItemPriority(item);
                        break;
                    }
                }
            }
        }

        Console.WriteLine(sum);
    }
}
