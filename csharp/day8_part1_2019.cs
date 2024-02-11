
using System;
using System.IO;
using System.Linq;

class Program
{
    static void Main()
    {
        string imageData = File.ReadAllText("input.txt").Trim();

        int width = 25;
        int height = 6;
        int layerSize = width * height;

        int minZeros = layerSize + 1;
        int result = 0;

        for (int i = 0; i < imageData.Length; i += layerSize)
        {
            string layer = imageData.Substring(i, Math.Min(layerSize, imageData.Length - i));
            int zeroCount = layer.Count(c => c == '0');
            int oneCount = layer.Count(c => c == '1');
            int twoCount = layer.Count(c => c == '2');

            if (zeroCount < minZeros)
            {
                minZeros = zeroCount;
                result = oneCount * twoCount;
            }
        }

        Console.WriteLine(result);
    }
}
