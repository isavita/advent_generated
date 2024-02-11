
using System;
using System.IO;
using System.Linq;

class Program
{
    static void Main()
    {
        string[] lengthsStr = File.ReadAllText("input.txt").Split(',');
        int[] lengths = lengthsStr.Select(int.Parse).ToArray();

        int[] list = Enumerable.Range(0, 256).ToArray();
        int currentPosition = 0;
        int skipSize = 0;

        foreach (int length in lengths)
        {
            for (int i = 0; i < length / 2; i++)
            {
                int start = (currentPosition + i) % 256;
                int end = (currentPosition + length - 1 - i) % 256;
                int temp = list[start];
                list[start] = list[end];
                list[end] = temp;
            }

            currentPosition = (currentPosition + length + skipSize) % 256;
            skipSize++;
        }

        int result = list[0] * list[1];
        Console.WriteLine(result);
    }
}
