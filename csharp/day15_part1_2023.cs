
using System;
using System.IO;
using System.Linq;

class Step
{
    public string Label { get; set; }
    public int NumBox { get; set; }
    public string Operation { get; set; }
    public int Number { get; set; }
}

class Program
{
    const int HashTableSize = 256;

    static int HashString(string str)
    {
        int res = 0;
        foreach (char c in str)
        {
            res += (int)c;
            res *= 17;
            res %= HashTableSize;
        }
        return res;
    }

    static Step ParseStep(string stepStr)
    {
        Step step = new Step();

        step.Label = stepStr.TrimEnd(new char[] { '=', '-', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' });
        step.NumBox = HashString(step.Label);
        step.Operation = stepStr.Substring(step.Label.Length, 1);
        if (step.Operation == "=")
        {
            step.Number = int.Parse(stepStr.Substring(step.Label.Length + 1));
        }

        return step;
    }

    static int Solve(string[] input)
    {
        string line = input[0];
        string[] steps = line.Split(',');
        int res = 0;
        foreach (string step in steps)
        {
            res += HashString(step);
        }
        return res;
    }

    static string[] ReadFile(string fileName)
    {
        return File.ReadAllLines(fileName);
    }

    static void Main()
    {
        string[] input = ReadFile("input.txt");
        Console.WriteLine(Solve(input));
    }
}
