
using System;
using System.IO;
using System.Text.RegularExpressions;
using System.Linq;

class Program
{
    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");
        Regex regex = new Regex(@"Game (\d+): (.+)");
        Regex cubeRegex = new Regex(@"(\d+) (red|green|blue)");
        int totalPower = 0;

        foreach (string line in lines)
        {
            Match match = regex.Match(line);

            if (match.Success)
            {
                string[] rounds = match.Groups[2].Value.Split(';');
                int maxRed = 0, maxGreen = 0, maxBlue = 0;

                foreach (string round in rounds)
                {
                    MatchCollection cubes = cubeRegex.Matches(round);
                    int red = 0, green = 0, blue = 0;

                    foreach (Match cube in cubes)
                    {
                        int count = int.Parse(cube.Groups[1].Value);
                        switch (cube.Groups[2].Value)
                        {
                            case "red":
                                red += count;
                                break;
                            case "green":
                                green += count;
                                break;
                            case "blue":
                                blue += count;
                                break;
                        }
                    }

                    maxRed = Math.Max(maxRed, red);
                    maxGreen = Math.Max(maxGreen, green);
                    maxBlue = Math.Max(maxBlue, blue);
                }

                int power = maxRed * maxGreen * maxBlue;
                totalPower += power;
            }
        }

        Console.WriteLine(totalPower);
    }
}
