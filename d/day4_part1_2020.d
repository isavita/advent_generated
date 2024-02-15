import std.stdio;
import std.array;
import std.algorithm;
import std.conv;
import std.file;

void main()
{
    string input = cast(string)std.file.read("input.txt");
    auto passports = input.split("\n\n").map!(a => a.split("\n").join(" "));

    int validCount = 0;
    foreach (passport; passports)
    {
        auto fields = passport.split(" ").map!(a => a.split(":")[0]);
        if (fields.canFind("byr") && fields.canFind("iyr") && fields.canFind("eyr") &&
            fields.canFind("hgt") && fields.canFind("hcl") && fields.canFind("ecl") &&
            fields.canFind("pid"))
        {
            validCount++;
        }
    }

    writeln(validCount);
}