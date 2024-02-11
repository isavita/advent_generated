
using System;
using System.IO;
using System.Linq;

class Program
{
    static bool ValidatePassword(string policy, string password)
    {
        var parts = policy.Split('-', ' ', ':');
        int min = int.Parse(parts[0]);
        int max = int.Parse(parts[1]);
        char character = parts[2][0];
        return (password[min - 1] == character) != (password[max - 1] == character);
    }

    static void Main()
    {
        int validCount = 0;
        string[] lines = File.ReadAllLines("input.txt");
        
        foreach (var line in lines)
        {
            var parts = line.Split(':');
            string policy = parts[0].Trim();
            string password = parts[1].Trim();
            if (ValidatePassword(policy, password))
            {
                validCount++;
            }
        }

        Console.WriteLine(validCount);
    }
}
