using System;
using System.IO;

class Program
{
    static void Main()
    {
        const int width = 25;
        const int height = 6;
        const int layerSize = width * height;
        var data = File.ReadAllText("input.txt").Trim();
        var fileSize = data.Length;
        var finalImage = new char[layerSize];
        for (int i = 0; i < layerSize; i++) finalImage[i] = '2';
        for (int i = 0; i < fileSize; i += layerSize)
        {
            var end = Math.Min(i + layerSize, fileSize);
            for (int j = 0; j < end - i; j++)
                if (finalImage[j] == '2')
                    finalImage[j] = data[i + j];
        }
        for (int y = 0; y < height; y++)
        {
            for (int x = 0; x < width; x++)
            {
                var p = finalImage[y * width + x];
                Console.Write(p == '0' ? ' ' : p == '1' ? '#' : ' ');
            }
            Console.WriteLine();
        }
    }
}