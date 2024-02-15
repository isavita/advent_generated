import std.stdio;
import std.file;
import std.conv;

void main()
{
    // Read input from file
    string input = cast(string)std.file.read("input.txt");

    // Convert input string to array of integers
    int[] digits;
    foreach (char c; input)
    {
        digits ~= to!int(c.to!string);
    }

    // Apply FFT algorithm for 100 phases
    foreach (_; 0 .. 100)
    {
        digits = applyFFT(digits);
    }

    // Output the first eight digits
    foreach (i; 0 .. 8)
    {
        write(digits[i]);
    }
    writeln();
}

int[] applyFFT(int[] input)
{
    int[] basePattern = [0, 1, 0, -1];
    int[] output = new int[input.length];
    foreach (i, _; input)
    {
        int sum = 0;
        foreach (j, val; input)
        {
            int patternValue = basePattern[((j + 1) / (i + 1)) % basePattern.length];
            sum += val * patternValue;
        }
        output[i] = abs(sum % 10);
    }
    return output;
}

int abs(int x)
{
    return x < 0 ? -x : x;
}