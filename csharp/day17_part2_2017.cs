
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string input = File.ReadAllText("input.txt");
        int steps = int.Parse(input.Trim());
        int currentPos = 0;
        int valueAfterZero = 0;

        for (int i = 1; i <= 50000000; i++)
        {
            currentPos = (currentPos + steps) % i;
            if (currentPos == 0)
            {
                valueAfterZero = i;
            }
            currentPos++;
        }

        Console.WriteLine(valueAfterZero);
    }
}
