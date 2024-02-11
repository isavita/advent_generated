
using System;
using System.IO;

class Program
{
    static void Main()
    {
        int[] x = { 1 };
        string[] lines = File.ReadAllLines("input.txt");
        foreach (string line in lines)
        {
            switch (line)
            {
                case "noop":
                    x = Append(x, x[x.Length - 1]);
                    break;
                default:
                    int n = int.Parse(line.Split(" ")[1]);
                    x = Append(Append(x, x[x.Length - 1]), x[x.Length - 1] + n);
                    break;
            }
        }

        int sum = 0;
        for (int i = 0; i < x.Length; i++)
        {
            if ((i - 19) % 40 == 0)
            {
                sum += (i + 1) * x[i];
            }
        }
        Console.WriteLine(sum);
    }

    static int[] Append(int[] array, int value)
    {
        Array.Resize(ref array, array.Length + 1);
        array[array.Length - 1] = value;
        return array;
    }
}
