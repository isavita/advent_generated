
using System;
using System.IO;
using System.Linq;

class Program
{
    static bool ValidatePassword(string policy, string password)
    {
        var parts = policy.Split('-', ' ');
        int min = int.Parse(parts[0]);
        int max = int.Parse(parts[1]);
        char character = parts[2][0];

        int count = password.Count(c => c == character);

        return count >= min && count <= max;
    }

    static void Main()
    {
        int validCount = 0;
        string[] lines = File.ReadAllLines("input.txt");

        foreach (var line in lines)
        {
            int index = line.IndexOf(':');
            string policy = line.Substring(0, index);
            string password = line.Substring(index + 2);

            if (ValidatePassword(policy, password))
            {
                validCount++;
            }
        }

        Console.WriteLine(validCount);
    }
}
