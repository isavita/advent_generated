
using System;
using System.IO;
using System.Linq;

class Program
{
    static void Main()
    {
        string[] passports = File.ReadAllText("input.txt").Split("\n\n");

        int validPassports = 0;
        string[] requiredFields = { "byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid" };

        foreach (string passport in passports)
        {
            if (IsValid(passport, requiredFields))
            {
                validPassports++;
            }
        }

        Console.WriteLine(validPassports);
    }

    static bool IsValid(string passport, string[] requiredFields)
    {
        return requiredFields.All(field => passport.Contains(field + ":"));
    }
}
