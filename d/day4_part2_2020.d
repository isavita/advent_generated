import std.stdio;
import std.file;
import std.array;
import std.conv;
import std.string;
import std.regex;
import std.algorithm; // Added import for map

void main()
{
    string data = cast(string) read("input.txt");
    string[] passports = data.split("\n\n");

    int validPassportsPart1 = 0;
    int validPassportsPart2 = 0;

    foreach (passport; passports)
    {
        auto fields = passport.split().map!(a => a.split(":")).array;

        bool byr = false, iyr = false, eyr = false, hgt = false, hcl = false, ecl = false, pid = false;
        bool byrValid = false, iyrValid = false, eyrValid = false, hgtValid = false, hclValid = false, eclValid = false, pidValid = false;

        foreach (field; fields)
        {
            string key = field[0];
            string value = field[1];

            switch (key)
            {
                case "byr":
                    byr = true;
                    int year = to!int(value);
                    if (year >= 1920 && year <= 2002)
                        byrValid = true;
                    break;
                case "iyr":
                    iyr = true;
                    int year = to!int(value);
                    if (year >= 2010 && year <= 2020)
                        iyrValid = true;
                    break;
                case "eyr":
                    eyr = true;
                    int year = to!int(value);
                    if (year >= 2020 && year <= 2030)
                        eyrValid = true;
                    break;
                case "hgt":
                    hgt = true;
                    if (value.endsWith("cm"))
                    {
                        int height = to!int(value[0..$-2]);
                        if (height >= 150 && height <= 193)
                            hgtValid = true;
                    }
                    else if (value.endsWith("in"))
                    {
                        int height = to!int(value[0..$-2]);
                        if (height >= 59 && height <= 76)
                            hgtValid = true;
                    }
                    break;
                case "hcl":
                    hcl = true;
                    if (value.matchFirst("#[0-9a-f]{6}"))
                        hclValid = true;
                    break;
                case "ecl":
                    ecl = true;
                    if (value == "amb" || value == "blu" || value == "brn" || value == "gry" || value == "grn" || value == "hzl" || value == "oth")
                        eclValid = true;
                    break;
                case "pid":
                    pid = true;
                    if (value.matchFirst("^[0-9]{9}$"))
                        pidValid = true;
                    break;
                case "cid":
                    break;
                default:
                    break;
            }
        }

        if (byr && iyr && eyr && hgt && hcl && ecl && pid)
        {
            validPassportsPart1++;
            if (byrValid && iyrValid && eyrValid && hgtValid && hclValid && eclValid && pidValid)
                validPassportsPart2++;
        }
    }

    writeln(validPassportsPart1);
    writeln(validPassportsPart2);
}