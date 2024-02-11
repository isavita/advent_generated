
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

class Program
{
    static void Main()
    {
        List<string> root = new List<string> { "" };
        Dictionary<string, int> dirs = new Dictionary<string, int>();
        Dictionary<string, int> files = new Dictionary<string, int>();
        List<string> curr = new List<string>();

        using (StreamReader sr = new StreamReader("input.txt"))
        {
            string line;
            while ((line = sr.ReadLine()) != null)
            {
                string[] txt = line.Split(' ');
                if (txt[0] == "$")
                {
                    if (txt[1] == "cd")
                    {
                        if (txt[2] == "/")
                        {
                            curr = root.ToList();
                        }
                        else if (txt[2] == "..")
                        {
                            curr.RemoveAt(curr.Count - 1);
                        }
                        else
                        {
                            curr.Add(txt[2]);
                        }
                        dirs[string.Join("/", curr)] = 0;
                    }
                }
                else
                {
                    if (txt[0] != "dir")
                    {
                        files[string.Join("/", curr.Concat(new[] { txt[1] }).ToArray())] = Convert.ToInt32(txt[0]);
                    }
                }
            }
        }

        foreach (var f in files)
        {
            string[] path = f.Key.Split('/');
            for (int i = 1; i < path.Length; i++)
            {
                dirs[string.Join("/", path.Take(i))] += f.Value;
            }
        }

        List<int> sortedSizes = dirs.Values.ToList();
        sortedSizes.Sort();

        int total = 70000000;
        int want = 30000000;
        int available = total - dirs[""];
        
        if (sortedSizes.Count > 0)
        {
            int index = Array.BinarySearch(sortedSizes.ToArray(), want - available);
            if (index >= 0)
            {
                Console.WriteLine(sortedSizes[index]);
            }
            else
            {
                int insertionPoint = ~index;
                if (insertionPoint < sortedSizes.Count)
                {
                    Console.WriteLine(sortedSizes[insertionPoint]);
                }
                else
                {
                    Console.WriteLine("Desired size not found.");
                }
            }
        }
        else
        {
            Console.WriteLine("No sizes available.");
        }
    }
}
