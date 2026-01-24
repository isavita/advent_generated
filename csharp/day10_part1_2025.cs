
using System;
using System.IO;
using System.Linq;

class Program
{
    const int MAXR = 64, MAXC = 64, MAXMACH = 128;
    static int R, C, nFree, minWeight;
    static int[,] matrix = new int[MAXR, MAXC + 1];
    static bool[] colIsPivot = new bool[MAXC];
    static int[] freeVars = new int[MAXC];
    static int[] target = new int[MAXR];
    static int[][] buttons = Enumerable.Range(0, MAXC).Select(_ => new int[MAXR]).ToArray();
    static int[] btnLen = new int[MAXC];

    static int Bits(int x) { int c = 0; while (x != 0) { x &= x - 1; c++; } return c; }

    static int GaussianEliminationMinWeight()
    {
        int pivotRow = 0;
        Array.Clear(colIsPivot, 0, C);
        for (int c = 0; c < C && pivotRow < R; c++)
        {
            int sel = -1;
            for (int r = pivotRow; r < R; r++)
                if (matrix[r, c] == 1) { sel = r; break; }
            if (sel == -1) continue;
            for (int k = 0; k <= C; k++)
            {
                int t = matrix[pivotRow, k];
                matrix[pivotRow, k] = matrix[sel, k];
                matrix[sel, k] = t;
            }
            for (int r = 0; r < R; r++)
                if (r != pivotRow && matrix[r, c] == 1)
                    for (int k = c; k <= C; k++)
                        matrix[r, k] ^= matrix[pivotRow, k];
            colIsPivot[c] = true;
            pivotRow++;
        }
        for (int r = pivotRow; r < R; r++)
            if (matrix[r, C] == 1) return -1;
        nFree = 0;
        for (int c = 0; c < C; c++) if (!colIsPivot[c]) freeVars[nFree++] = c;
        minWeight = int.MaxValue;
        int limit = 1 << nFree;
        for (int i = 0; i < limit; i++)
        {
            int[] x = new int[MAXC];
            int cw = Bits(i);
            for (int j = 0; j < nFree; j++)
                if (((i >> j) & 1) == 1) x[freeVars[j]] = 1;
            int currPivotRow = 0;
            for (int c = 0; c < C; c++)
            {
                if (colIsPivot[c])
                {
                    int val = matrix[currPivotRow, C];
                    for (int k = c + 1; k < C; k++)
                        if (matrix[currPivotRow, k] == 1) val ^= x[k];
                    x[c] = val;
                    if (val == 1) cw++;
                    currPivotRow++;
                }
            }
            if (cw < minWeight) minWeight = cw;
        }
        return minWeight;
    }

    static void Main()
    {
        int totalPresses = 0;
        foreach (var line in File.ReadLines("input.txt"))
        {
            int start = line.IndexOf('[');
            if (start == -1) continue;
            int end = line.IndexOf(']', start);
            if (end == -1) continue;
            string config = line.Substring(start + 1, end - start - 1);
            R = config.Length;
            for (int i = 0; i < R; i++) target[i] = config[i] == '#' ? 1 : 0;
            C = 0;
            int p = end + 1;
            while ((p = line.IndexOf('(', p)) != -1)
            {
                int e2 = line.IndexOf(')', p);
                if (e2 == -1) break;
                string[] parts = line.Substring(p + 1, e2 - p - 1).Split(',');
                btnLen[C] = 0;
                foreach (var tok in parts)
                    buttons[C][btnLen[C]++] = int.Parse(tok);
                C++;
                p = e2 + 1;
            }
            for (int r = 0; r < R; r++)
            {
                for (int c = 0; c < C; c++)
                {
                    matrix[r, c] = 0;
                    for (int k = 0; k < btnLen[c]; k++)
                        if (buttons[c][k] == r) { matrix[r, c] = 1; break; }
                }
                matrix[r, C] = target[r];
            }
            int mw = GaussianEliminationMinWeight();
            if (mw != -1) totalPresses += mw;
        }
        Console.WriteLine(totalPresses);
    }
}
