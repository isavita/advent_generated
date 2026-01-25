
using System;
using System.IO;
using System.Linq;

public class Program
{
    public static void Main()
    {
        var input = File.ReadAllText("input.txt");
        var mirrors = input.Split(new[] { Environment.NewLine + Environment.NewLine }, StringSplitOptions.None)
            .Select(mirror => mirror.Split(new[] { Environment.NewLine }, StringSplitOptions.None));

        var result = mirrors.Sum(mirror => GetMirrorValue(mirror));
        Console.WriteLine(result);
    }

    private static int GetMirrorValue(string[] mirror)
    {
        var rows = mirror.Select(GetIntFromRow).ToArray();
        var cols = GetColumns(mirror).Select(GetIntFromRow).ToArray();

        var rowMirror = GetMirrorAxis(rows);
        var colMirror = GetMirrorAxis(cols);

        return colMirror + rowMirror * 100;
    }

    private static int GetIntFromRow(string row)
    {
        return row.Aggregate(0, (acc, c) => (acc << 1) + (c == '#' ? 1 : 0));
    }

    private static string[] GetColumns(string[] mirror)
    {
        var cols = new string[mirror[0].Length];
        for (int i = 0; i < mirror[0].Length; i++)
        {
            cols[i] = new string(mirror.Select(row => row[i]).ToArray());
        }
        return cols;
    }

    private static int GetMirrorAxis(int[] values)
    {
        for (int i = 1; i < values.Length; i++)
        {
            if (IsMirror(values, i))
            {
                return i;
            }
        }
        return 0;
    }

    private static bool IsMirror(int[] values, int axis)
    {
        for (int j = 0; j < Math.Min(axis, values.Length - axis); j++)
        {
            if (values[axis - 1 - j] != values[axis + j])
            {
                return false;
            }
        }
        return true;
    }
}
