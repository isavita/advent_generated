
import std.file : readText;
import std.conv : to;
import std.string : split, strip;
import std.algorithm : min;
import std.stdio : writeln;

void main()
{
    string input = readText("input.txt").strip;
    int totalWrappingPaper = 0;

    foreach (line; input.split("\n"))
    {
        int l = to!int(line.split("x")[0]);
        int w = to!int(line.split("x")[1]);
        int h = to!int(line.split("x")[2]);

        int side1 = l * w;
        int side2 = w * h;
        int side3 = h * l;

        int slack = min(side1, min(side2, side3));
        int surfaceArea = 2*l*w + 2*w*h + 2*h*l;

        totalWrappingPaper += surfaceArea + slack;
    }

    writeln(totalWrappingPaper);
}
