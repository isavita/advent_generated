import std.file : read;
import std.stdio : writeln;

void main()
{
    string input = cast(string) read("input.txt");
    
    int width = 25;
    int height = 6;
    int layerSize = width * height;
    
    int minZeros = layerSize + 1;
    int result = 0;
    
    for (int i = 0; i < input.length; i += layerSize)
    {
        int zeros = 0;
        int ones = 0;
        int twos = 0;
        
        foreach (j; 0 .. layerSize)
        {
            char pixel = input[i + j];
            if (pixel == '0')
                zeros++;
            else if (pixel == '1')
                ones++;
            else if (pixel == '2')
                twos++;
        }
        
        if (zeros < minZeros)
        {
            minZeros = zeros;
            result = ones * twos;
        }
    }
    
    writeln(result);
}