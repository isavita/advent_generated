
using System;
using System.IO;

class Program
{
    static void Main()
    {
        int sum = 0;
        foreach (string line in File.ReadLines("input.txt"))
        {
            int first = -1, last = 0;
            foreach (char c in line)
            {
                if (c >= '0' && c <= '9')
                {
                    int d = c - '0';
                    if (first < 0) first = d;
                    last = d;
                }
            }
            if (first >= 0) sum += first * 10 + last;
        }
        Console.WriteLine(sum);
    }
}
