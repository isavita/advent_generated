
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string input = File.ReadAllText("input.txt").Trim();
        int floor = 0;
        int position = 0;
        for (int i = 0; i < input.Length; i++)
        {
            if (input[i] == '(')
            {
                floor++;
            }
            else if (input[i] == ')')
            {
                floor--;
            }
            if (floor == -1)
            {
                position = i + 1;
                break;
            }
        }
        Console.WriteLine(position);
    }
}
