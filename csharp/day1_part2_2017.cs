
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string input = File.ReadAllText("input.txt").Trim();
        int halfway = input.Length / 2;
        int sum = 0;

        for (int i = 0; i < input.Length; i++)
        {
            int next = (i + halfway) % input.Length;
            if (input[i] == input[next])
            {
                sum += (int)(input[i] - '0');
            }
        }

        Console.WriteLine(sum);
    }
}
