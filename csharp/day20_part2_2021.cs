
using System;
using System.IO;
using System.Linq;

class Program
{
    const int iterations = 50;
    const int expandBy = 1;

    static void Main()
    {
        var input = File.ReadAllLines("input.txt");
        var algorithm = input[0];
        var image = input.Skip(2).Select(line => line.Select(c => c == '#').ToArray()).ToArray();

        for (int i = 0; i < iterations; i++)
        {
            image = EnhanceImage(algorithm, image, i % 2 == 1 && algorithm[0] == '#');
        }

        Console.WriteLine(CountLitPixels(image));
    }

    static bool[][] EnhanceImage(string algorithm, bool[][] image, bool useInfiniteLit)
    {
        var newImage = new bool[image.Length + (expandBy * 2)][];
        for (int i = 0; i < newImage.Length; i++)
        {
            newImage[i] = new bool[image[0].Length + (expandBy * 2)];
        }

        for (int y = -expandBy; y < image.Length + expandBy; y++)
        {
            for (int x = -expandBy; x < image[0].Length + expandBy; x++)
            {
                int index = 0;
                for (int dy = -1; dy <= 1; dy++)
                {
                    for (int dx = -1; dx <= 1; dx++)
                    {
                        index <<= 1;
                        int ny = y + dy, nx = x + dx;
                        if (ny >= 0 && ny < image.Length && nx >= 0 && nx < image[0].Length)
                        {
                            if (image[ny][nx])
                            {
                                index |= 1;
                            }
                        }
                        else if (useInfiniteLit)
                        {
                            index |= 1;
                        }
                    }
                }
                newImage[y + expandBy][x + expandBy] = algorithm[index] == '#';
            }
        }

        return newImage;
    }

    static int CountLitPixels(bool[][] image)
    {
        return image.SelectMany(row => row).Count(pixel => pixel);
    }
}
