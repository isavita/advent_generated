using System;
using System.IO;

class Program
{
    static void Main()
    {
        string s = File.ReadAllText("input.txt");
        long sum = 0, num = 0;
        bool neg = false;
        foreach (char c in s)
        {
            if (c == '-')
                neg = true;
            else if (c >= '0' && c <= '9')
                num = num * 10 + (c - '0');
            else
            {
                if (neg) num = -num;
                sum += num;
                num = 0;
                neg = false;
            }
        }
        if (neg) num = -num;
        sum += num;
        Console.WriteLine(sum);
    }
}