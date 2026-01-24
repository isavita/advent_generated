
using System;
using System.IO;

class Program
{
    static void Main()
    {
        long total = 0;
        foreach (var line in File.ReadLines("input.txt"))
        {
            ulong s = ulong.Parse(line);
            for (int i = 0; i < 2000; i++)
            {
                s ^= s * 64;
                s &= 0xFFFFFF;
                s ^= s / 32;
                s &= 0xFFFFFF;
                s ^= s * 2048;
                s &= 0xFFFFFF;
            }
            total += (long)s;
        }
        Console.WriteLine(total);
    }
}
