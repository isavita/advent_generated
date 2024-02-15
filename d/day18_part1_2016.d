
import std.stdio;
import std.file;
import std.array;
import std.conv;

const int totalRows = 40;

void main()
{
    string firstRow = readFirstRow("input.txt");
    int safeTilesCount = countSafeTiles(firstRow, totalRows);
    writeln(safeTilesCount);
}

string readFirstRow(string filename)
{
    auto file = File(filename, "r");
    string firstRow = file.readln();
    file.close();
    return firstRow;
}

int countSafeTiles(string firstRow, int totalRows)
{
    string currentRow = firstRow;
    int safeCount = countChar(currentRow, '.');

    foreach (i; 1..totalRows)
    {
        string nextRow;
        foreach (j; 0..currentRow.length)
        {
            if (isTrap(cast(int)(j-1), cast(int)(j), cast(int)(j+1), currentRow))
            {
                nextRow ~= "^";
            }
            else
            {
                nextRow ~= ".";
                safeCount++;
            }
        }
        currentRow = nextRow;
    }
    return safeCount;
}

bool isTrap(int left, int center, int right, string row)
{
    char l = safeIfOutOfBounds(left, row);
    char c = row[center];
    char r = safeIfOutOfBounds(right, row);

    return (l == '^' && c == '^' && r == '.') ||
           (c == '^' && r == '^' && l == '.') ||
           (l == '^' && c == '.' && r == '.') ||
           (r == '^' && c == '.' && l == '.');
}

char safeIfOutOfBounds(int index, string row)
{
    if (index < 0 || index >= row.length)
    {
        return '.';
    }
    return row[index];
}

int countChar(string str, char character)
{
    int count = 0;
    foreach (char c; str)
    {
        if (c == character)
        {
            count++;
        }
    }
    return count;
}
