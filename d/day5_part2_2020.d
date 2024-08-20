import std.stdio;
import std.file;
import std.conv;
import std.algorithm; // Added import for sort

void main()
{
    auto file = File("input.txt", "r");
    int highestSeatID = 0;
    int[] seatIDs;

    foreach(line; file.byLine())
    {
        int row = 0, col = 0;
        foreach(i, c; line)
        {
            if(i < 7)
                row = (row << 1) | (c == 'B' ? 1 : 0);
            else
                col = (col << 1) | (c == 'R' ? 1 : 0);
        }
        int seatID = row * 8 + col;
        seatIDs ~= seatID;
        if(seatID > highestSeatID)
            highestSeatID = seatID;
    }

    writeln("Highest Seat ID: ", highestSeatID);

    seatIDs.sort();
    foreach(i; 1 .. seatIDs.length)
    {
        if(seatIDs[i] != seatIDs[i-1] + 1)
        {
            writeln("Your Seat ID: ", seatIDs[i-1] + 1);
            break;
        }
    }
}