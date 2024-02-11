
using System;
using System.IO;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");
        Regex regex = new Regex(@"Game (\d+): (.+)");
        Regex cubeRegex = new Regex(@"(\d+) (red|green|blue)");
        int totalSum = 0;

        foreach (string line in lines)
        {
            Match match = regex.Match(line);

            if (match.Success)
            {
                int gameId = int.Parse(match.Groups[1].Value);
                string[] rounds = match.Groups[2].Value.Split(';');
                bool isValid = true;

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

                        if (red > 12 || green > 13 || blue > 14)
                        {
                            isValid = false;
                            break;
                        }
                    }

                    if (!isValid)
                    {
                        break;
                    }
                }

                if (isValid)
                {
                    totalSum += gameId;
                }
            }
        }

        Console.WriteLine(totalSum);
    }
}
