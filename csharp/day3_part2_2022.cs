
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
        int groupLineCounter = 0;
        Dictionary<char, int>[] groupItems = new Dictionary<char, int>[3];

        using (StreamReader sr = new StreamReader("input.txt"))
        {
            string line;
            while ((line = sr.ReadLine()) != null)
            {
                Dictionary<char, int> itemsMap = new Dictionary<char, int>();
                foreach (char item in line)
                {
                    if (itemsMap.ContainsKey(item))
                    {
                        itemsMap[item]++;
                    }
                    else
                    {
                        itemsMap[item] = 1;
                    }
                }
                groupItems[groupLineCounter] = itemsMap;
                groupLineCounter++;

                if (groupLineCounter == 3)
                {
                    Dictionary<char, int> commonItems = new Dictionary<char, int>();
                    foreach (char item in groupItems[0].Keys)
                    {
                        if (groupItems[1].ContainsKey(item) && groupItems[2].ContainsKey(item))
                        {
                            commonItems[item] = 1;
                        }
                    }
                    foreach (char item in commonItems.Keys)
                    {
                        sum += ItemPriority(item);
                        break; // Since we need only one common item per group
                    }
                    groupLineCounter = 0;
                }
            }
        }

        Console.WriteLine(sum);
    }
}
