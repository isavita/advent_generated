
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string[] forest = File.ReadAllLines("input.txt");
        int trees = CountTrees(forest, 3, 1);
        Console.WriteLine(trees);
    }

    static int CountTrees(string[] forest, int right, int down)
    {
        int trees = 0;
        int x = 0;
        int width = forest[0].Length;

        for (int y = 0; y < forest.Length; y += down)
        {
            if (forest[y][x % width] == '#')
            {
                trees++;
            }
            x += right;
        }

        return trees;
    }
}
