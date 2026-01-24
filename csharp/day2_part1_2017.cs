
using System;
using System.IO;

class Program
{
    static void Main()
    {
        long checksum = 0;
        foreach (var line in File.ReadLines("input.txt"))
        {
            int min = int.MaxValue, max = int.MinValue, num = 0;
            int i = 0, len = line.Length;
            while (i <= len)
            {
                if (i == len || line[i] == ' ' || line[i] == '\t')
                {
                    if (i > 0 && (line[i - 1] != ' ' && line[i - 1] != '\t'))
                    {
                        if (num < min) min = num;
                        if (num > max) max = num;
                    }
                    num = 0;
                }
                else
                {
                    num = num * 10 + (line[i] - '0');
                }
                i++;
            }
            checksum += max - min;
        }
        Console.WriteLine(checksum);
    }
}
