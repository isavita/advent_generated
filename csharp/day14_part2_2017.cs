
using System;
using System.IO;
using System.Text;

class Program
{
    static void ReverseSection(int[] arr, int start, int length)
    {
        int n = arr.Length;
        for (int i = start, j = start + length - 1; i < j; i++, j--)
        {
            int tmp = arr[i % n];
            arr[i % n] = arr[j % n];
            arr[j % n] = tmp;
        }
    }

    static string KnotHash(string input)
    {
        int[] lengths = new int[input.Length];
        for (int i = 0; i < input.Length; i++)
        {
            lengths[i] = (int)input[i];
        }
        int[] extraLengths = { 17, 31, 73, 47, 23 };
        Array.Resize(ref lengths, lengths.Length + extraLengths.Length);
        extraLengths.CopyTo(lengths, input.Length);

        int[] list = new int[256];
        for (int i = 0; i < list.Length; i++)
        {
            list[i] = i;
        }

        int position = 0;
        int skip = 0;
        for (int round = 0; round < 64; round++)
        {
            foreach (int length in lengths)
            {
                ReverseSection(list, position, length);
                position += length + skip;
                skip++;
            }
        }

        int[] denseHash = new int[16];
        for (int i = 0; i < 16; i++)
        {
            int xor = 0;
            for (int j = 0; j < 16; j++)
            {
                xor ^= list[i * 16 + j];
            }
            denseHash[i] = xor;
        }

        StringBuilder hexHash = new StringBuilder();
        foreach (int v in denseHash)
        {
            hexHash.Append(v.ToString("x2"));
        }
        return hexHash.ToString();
    }

    static string HexToBinary(string hexStr)
    {
        StringBuilder binaryStr = new StringBuilder();
        foreach (char hexDigit in hexStr)
        {
            int val = Convert.ToInt32(hexDigit.ToString(), 16);
            binaryStr.Append(Convert.ToString(val, 2).PadLeft(4, '0'));
        }
        return binaryStr.ToString();
    }

    static void DFS(int x, int y, int[][] grid)
    {
        if (x < 0 || x >= 128 || y < 0 || y >= 128 || grid[x][y] != 1)
        {
            return;
        }
        grid[x][y] = 0;
        DFS(x - 1, y, grid);
        DFS(x + 1, y, grid);
        DFS(x, y - 1, grid);
        DFS(x, y + 1, grid);
    }

    static void Main()
    {
        string keyString = File.ReadAllText("input.txt").Trim();
        int[][] grid = new int[128][];
        int totalUsed = 0;
        int regions = 0;

        for (int i = 0; i < 128; i++)
        {
            grid[i] = new int[128];
            string rowKey = keyString + "-" + i;
            string hash = KnotHash(rowKey);
            string binaryRow = HexToBinary(hash);

            for (int j = 0; j < binaryRow.Length; j++)
            {
                if (binaryRow[j] == '1')
                {
                    grid[i][j] = 1;
                    totalUsed++;
                }
            }
        }

        for (int i = 0; i < 128; i++)
        {
            for (int j = 0; j < 128; j++)
            {
                if (grid[i][j] == 1)
                {
                    regions++;
                    DFS(i, j, grid);
                }
            }
        }

        Console.WriteLine(regions);
    }
}
