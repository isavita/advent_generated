
import std.stdio;
import std.file;
import std.algorithm;

void main()
{
    string firstRow = cast(string)std.file.readText("input.txt").splitter().front;
    int totalRows = 400000;
    int safeTilesCount = countSafeTiles(firstRow, totalRows);
    writeln(safeTilesCount);
}

int countSafeTiles(string firstRow, int totalRows)
{
    string currentRow = firstRow;
    int safeCount = countChar(currentRow, '.');

    foreach (i; 1 .. totalRows)
    {
        string nextRow;
        foreach (j; 0 .. currentRow.length)
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

int countChar(string str, char ch)
{
    return cast(int)count(str, ch);
}
