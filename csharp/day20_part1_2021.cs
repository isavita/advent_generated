
using System;
using System.IO;
using System.Linq;

class Program
{
    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");
        string algorithm = lines[0];
        char[][] image = lines.Skip(2).Select(line => line.ToCharArray()).ToArray();
        image = EnhanceImage(image, algorithm, 2);
        Console.WriteLine(CountLitPixels(image));
    }

    static char[][] EnhanceImage(char[][] image, string algorithm, int times)
    {
        for (int i = 0; i < times; i++)
        {
            image = ApplyAlgorithm(image, algorithm, i % 2 == 1 && algorithm[0] == '#');
        }
        return image;
    }

    static char[][] ApplyAlgorithm(char[][] image, string algorithm, bool flip)
    {
        char[][] enhancedImage = new char[image.Length + 2][];
        for (int i = 0; i < enhancedImage.Length; i++)
        {
            enhancedImage[i] = new char[image[0].Length + 2];
            for (int j = 0; j < enhancedImage[i].Length; j++)
            {
                int index = CalculateIndex(i - 1, j - 1, image, flip);
                enhancedImage[i][j] = algorithm[index];
            }
        }
        return enhancedImage;
    }

    static int CalculateIndex(int i, int j, char[][] image, bool flip)
    {
        int index = 0;
        for (int di = -1; di <= 1; di++)
        {
            for (int dj = -1; dj <= 1; dj++)
            {
                index <<= 1;
                if (i + di >= 0 && i + di < image.Length && j + dj >= 0 && j + dj < image[0].Length)
                {
                    if (image[i + di][j + dj] == '#')
                    {
                        index |= 1;
                    }
                }
                else if (flip)
                {
                    index |= 1;
                }
            }
        }
        return index;
    }

    static int CountLitPixels(char[][] image)
    {
        int count = 0;
        foreach (var row in image)
        {
            foreach (var pixel in row)
            {
                if (pixel == '#')
                {
                    count++;
                }
            }
        }
        return count;
    }
}
