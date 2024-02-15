
import std.file;
import std.conv;
import std.stdio;

void main() {
    string input = cast(string) read("input.txt");
    
    foreach (_; 0 .. 40) {
        string result = "";
        char currentChar = input[0];
        int count = 1;
        
        foreach (i, c; input[1 .. $]) {
            if (c == currentChar) {
                count++;
            } else {
                result ~= count.to!string ~ currentChar;
                currentChar = c;
                count = 1;
            }
        }
        
        result ~= count.to!string ~ currentChar;
        input = result;
    }
    
    writeln(input.length);
}
