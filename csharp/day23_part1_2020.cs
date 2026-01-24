
using System;
using System.IO;

class Program
{
    const int MAX_CUPS = 1_000_000;
    static int[] cups = new int[MAX_CUPS];

    static void Main()
    {
        string input = File.ReadAllText("input.txt").Trim();
        int len = input.Length;
        int current = input[0] - '0';

        for (int i = 0; i < len; i++)
        {
            int cup = input[i] - '0';
            cups[cup] = i < len - 1 ? input[i + 1] - '0' : input[0] - '0';
        }

        for (int move = 0; move < 100; move++)
        {
            int p1 = cups[current];
            int p2 = cups[p1];
            int p3 = cups[p2];

            cups[current] = cups[p3];

            int dest = current - 1;
            if (dest < 1) dest = len;
            while (dest == p1 || dest == p2 || dest == p3)
                if (--dest < 1) dest = len;

            cups[p3] = cups[dest];
            cups[dest] = p1;

            current = cups[current];
        }

        int c = cups[1];
        while (c != 1)
        {
            Console.Write(c);
            c = cups[c];
        }
        Console.WriteLine();
    }
}
