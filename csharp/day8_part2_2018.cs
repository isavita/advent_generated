
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

        int value = ParseTree(numbers, 0).Item1;
        Console.WriteLine(value);
    }

    static (int, int) ParseTree(int[] data, int index)
    {
        int childCount = data[index];
        int metaCount = data[index + 1];
        index += 2;

        int[] childValues = new int[childCount];
        for (int i = 0; i < childCount; i++)
        {
            var result = ParseTree(data, index);
            childValues[i] = result.Item1;
            index = result.Item2;
        }

        int value = 0;
        if (childCount == 0)
        {
            for (int i = 0; i < metaCount; i++)
            {
                value += data[index + i];
            }
        }
        else
        {
            for (int i = 0; i < metaCount; i++)
            {
                int metadata = data[index + i];
                if (metadata <= childCount && metadata > 0)
                {
                    value += childValues[metadata - 1];
                }
            }
        }
        index += metaCount;

        return (value, index);
    }
}
