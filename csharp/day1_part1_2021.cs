
using System;
using System.IO;

class Program
{
    static void Main()
    {
        StreamReader file = new StreamReader("input.txt");
        int prev = 0, current, count = 0;

        while (!file.EndOfStream)
        {
            current = int.Parse(file.ReadLine());
            if (prev != 0 && current > prev)
            {
                count++;
            }
            prev = current;
        }

        Console.WriteLine(count);
        file.Close();
    }
}
