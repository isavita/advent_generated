
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");
        int[] expenses = Array.ConvertAll(lines, int.Parse);

        for (int i = 0; i < expenses.Length; i++)
        {
            for (int j = i + 1; j < expenses.Length; j++)
            {
                for (int k = j + 1; k < expenses.Length; k++)
                {
                    if (expenses[i] + expenses[j] + expenses[k] == 2020)
                    {
                        Console.WriteLine(expenses[i] * expenses[j] * expenses[k]);
                        return;
                    }
                }
            }
        }
    }
}
