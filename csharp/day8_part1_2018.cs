
using System;
using System.IO;
using System.Linq;

class Program
{
    static void Main()
    {
        int[] numbers = File.ReadAllText("input.txt")
            .Split(' ')
            .Select(int.Parse)
            .ToArray();

        int sum = ParseTree(numbers, 0, out _);
        Console.WriteLine(sum);
    }

    static int ParseTree(int[] data, int index, out int newIndex)
    {
        int childCount = data[index];
        int metaCount = data[index + 1];
        index += 2;

        int sum = 0;
        for (int i = 0; i < childCount; i++)
        {
            int childSum = ParseTree(data, index, out newIndex);
            sum += childSum;
            index = newIndex;
        }

        for (int i = 0; i < metaCount; i++)
        {
            sum += data[index + i];
        }
        index += metaCount;

        newIndex = index;
        return sum;
    }
}
