
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string input = File.ReadAllText("input.txt").Trim();
        int floor = 0;
        foreach (char c in input)
        {
            if (c == '(')
            {
                floor++;
            }
            else if (c == ')')
            {
                floor--;
            }
        }
        Console.WriteLine(floor);
    }
}
