
import std.stdio;
import std.file;

void main()
{
    string initialState = readInitialState("input.txt");
    string data = generateData(initialState, 35651584);
    string checksum = calculateChecksum(data);
    writeln("Checksum:", checksum);
}

string readInitialState(string filename)
{
    string initialState;
    File file = File(filename, "r");
    foreach(line; file.byLine())
    {
        initialState = line.idup;
        break;
    }
    return initialState;
}

string generateData(string initialState, int length)
{
    string data = initialState;
    while(data.length < length)
    {
        string b;
        foreach_reverse(c; data)
        {
            if(c == '0')
            {
                b ~= '1';
            }
            else
            {
                b ~= '0';
            }
        }
        data ~= "0" ~ b;
    }
    return data[0..length];
}

string calculateChecksum(string data)
{
    while(data.length % 2 == 0)
    {
        string b;
        for(int i = 0; i < data.length; i += 2)
        {
            if(data[i] == data[i + 1])
            {
                b ~= '1';
            }
            else
            {
                b ~= '0';
            }
        }
        data = b;
    }
    return data;
}
