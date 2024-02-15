import std.stdio;
import std.file;
import std.string;

void main()
{
    string initialState = readInitialState("input.txt");
    string data = generateData(initialState, 272);
    string checksum = calculateChecksum(data);
    writeln("Checksum:", checksum);
}

string readInitialState(string filename)
{
    string initialState;
    try
    {
        auto file = File(filename, "r");
        initialState = file.readln();
    }
    catch (Exception e)
    {
        stderr.writeln("Failed to read initial state");
        throw e;
    }
    return initialState;
}

string generateData(string initialState, int length)
{
    string data = initialState;
    while (data.length < length)
    {
        string b;
        foreach_reverse (c; data)
        {
            b ~= (c == '0') ? '1' : '0';
        }
        data ~= '0' ~ b;
    }
    return data[0..length];
}

string calculateChecksum(string data)
{
    while (data.length % 2 == 0)
    {
        string b;
        for (size_t i = 0; i < data.length; i += 2)
        {
            b ~= (data[i] == data[i + 1]) ? '1' : '0';
        }
        data = b;
    }
    return data;
}