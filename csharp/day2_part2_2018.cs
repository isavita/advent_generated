
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");
        for (int i = 0; i < lines.Length - 1; i++)
        {
            for (int j = i + 1; j < lines.Length; j++)
            {
                int diff = 0;
                for (int k = 0; k < lines[i].Length; k++)
                {
                    if (lines[i][k] != lines[j][k])
                    {
                        diff++;
                        if (diff > 1)
                        {
                            break;
                        }
                    }
                }
                if (diff == 1)
                {
                    string common = "";
                    for (int k = 0; k < lines[i].Length; k++)
                    {
                        if (lines[i][k] == lines[j][k])
                        {
                            common += lines[i][k];
                        }
                    }
                    Console.WriteLine(common);
                    return;
                }
            }
        }
    }
}
