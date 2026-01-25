
using System;
using System.Collections.Generic;
using System.IO;

class Program
{
    const int BlockHeight = 7;
    const int ShapeWidth = 5;
    const int MaxHeight = 6;          // rows 0‑5 (inclusive)
    const int MaxSum = MaxHeight - 1; // 5

    static void Main()
    {
        const string fileName = "input.txt";
        if (!File.Exists(fileName))
        {
            Console.WriteLine("0");
            return;
        }

        var locks = new List<int[]>();
        var keys  = new List<int[]>();

        int totalValid = 0;
        var block = new string[BlockHeight];
        int lineIdx = 0;
        bool blockInvalid = false;

        foreach (var rawLine in File.ReadLines(fileName))
        {
            // remove trailing CR/LF and spaces
            var line = rawLine.TrimEnd('\r', '\n', ' ');
            if (line.Length == 0) continue;               // skip empty lines
            totalValid++;

            if (line.Length < ShapeWidth) blockInvalid = true;
            block[lineIdx++] = line;

            if (lineIdx == BlockHeight)
            {
                if (!blockInvalid)
                {
                    bool isLock = true;
                    for (int c = 0; c < ShapeWidth; c++)
                        if (block[0][c] != '#') { isLock = false; break; }

                    if (isLock)
                        locks.Add(ParseLock(block));
                    else
                        keys.Add(ParseKey(block));
                }
                // reset for next block
                lineIdx = 0;
                blockInvalid = false;
            }
        }

        // incomplete block or no data → answer 0
        if (totalValid == 0 || totalValid % BlockHeight != 0)
        {
            Console.WriteLine("0");
            return;
        }

        long count = 0;
        foreach (var l in locks)
            foreach (var k in keys)
                if (Fits(l, k)) count++;

        Console.WriteLine(count);
    }

    static int[] ParseLock(string[] b)
    {
        var res = new int[ShapeWidth];
        for (int c = 0; c < ShapeWidth; c++)
        {
            int cnt = 0;
            // rows 1 .. 6 (inclusive)
            for (int r = 1; r < BlockHeight; r++)
            {
                if (b[r][c] == '#') cnt++;
                else break;
            }
            res[c] = cnt;
        }
        return res;
    }

    static int[] ParseKey(string[] b)
    {
        var res = new int[ShapeWidth];
        for (int c = 0; c < ShapeWidth; c++)
        {
            int cnt = 0;
            // rows 5 .. 0 (inclusive)
            for (int r = MaxHeight - 1; r >= 0; r--)
            {
                if (b[r][c] == '#') cnt++;
                else break;
            }
            res[c] = cnt;
        }
        return res;
    }

    static bool Fits(int[] lockArr, int[] keyArr)
    {
        for (int i = 0; i < ShapeWidth; i++)
            if (lockArr[i] + keyArr[i] > MaxSum) return false;
        return true;
    }
}
