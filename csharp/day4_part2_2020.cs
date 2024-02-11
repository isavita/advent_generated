
using System;
using System.IO;
using System.Text.RegularExpressions;
using System.Collections.Generic;

class Program
{
    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");
        string passport = "";
        int validPassports = 0;

        foreach (string line in lines)
        {
            if (line == "")
            {
                if (IsValidPassport(passport))
                {
                    validPassports++;
                }
                passport = "";
            }
            else
            {
                passport += " " + line;
            }
        }

        if (passport != "")
        {
            if (IsValidPassport(passport))
            {
                validPassports++;
            }
        }

        Console.WriteLine(validPassports);
    }

    static bool IsValidPassport(string passport)
    {
        string[] fields = passport.Split(' ', StringSplitOptions.RemoveEmptyEntries);
        var fieldMap = new Dictionary<string, string>();

        foreach (string field in fields)
        {
            string[] parts = field.Split(':');
            if (parts.Length == 2)
            {
                fieldMap[parts[0]] = parts[1];
            }
        }

        return fieldMap.ContainsKey("byr") &&
            ValidateByr(fieldMap["byr"]) &&
            fieldMap.ContainsKey("iyr") &&
            ValidateIyr(fieldMap["iyr"]) &&
            fieldMap.ContainsKey("eyr") &&
            ValidateEyr(fieldMap["eyr"]) &&
            fieldMap.ContainsKey("hgt") &&
            ValidateHgt(fieldMap["hgt"]) &&
            fieldMap.ContainsKey("hcl") &&
            ValidateHcl(fieldMap["hcl"]) &&
            fieldMap.ContainsKey("ecl") &&
            ValidateEcl(fieldMap["ecl"]) &&
            fieldMap.ContainsKey("pid") &&
            ValidatePid(fieldMap["pid"]);
    }

    static bool ValidateByr(string value)
    {
        return ValidateYear(value, 1920, 2002);
    }

    static bool ValidateIyr(string value)
    {
        return ValidateYear(value, 2010, 2020);
    }

    static bool ValidateEyr(string value)
    {
        return ValidateYear(value, 2020, 2030);
    }

    static bool ValidateYear(string value, int min, int max)
    {
        if (int.TryParse(value, out int year))
        {
            return year >= min && year <= max;
        }
        return false;
    }

    static bool ValidateHgt(string value)
    {
        if (value.EndsWith("cm"))
        {
            if (int.TryParse(value.Replace("cm", ""), out int hgt))
            {
                return hgt >= 150 && hgt <= 193;
            }
        }
        else if (value.EndsWith("in"))
        {
            if (int.TryParse(value.Replace("in", ""), out int hgt))
            {
                return hgt >= 59 && hgt <= 76;
            }
        }
        return false;
    }

    static bool ValidateHcl(string value)
    {
        return Regex.IsMatch(value, @"^#[0-9a-f]{6}$");
    }

    static bool ValidateEcl(string value)
    {
        string[] validEcl = { "amb", "blu", "brn", "gry", "grn", "hzl", "oth" };
        return Array.Exists(validEcl, e => e == value);
    }

    static bool ValidatePid(string value)
    {
        return Regex.IsMatch(value, @"^[0-9]{9}$");
    }
}
