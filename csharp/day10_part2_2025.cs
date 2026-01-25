using System;
using System.IO;
using System.Collections.Generic;

class Program
{
    const int MAX_COUNTERS = 20;
    const int MAX_BUTTONS = 50;
    const int INF = 0x3f3f3f3f;

    static int[,] buttons = new int[MAX_BUTTONS, MAX_COUNTERS];
    static int[] btnSize = new int[MAX_BUTTONS];
    static int[] targets = new int[MAX_COUNTERS];
    static int numCounters, numButtons;

    static double[,] matrix = new double[MAX_COUNTERS, MAX_BUTTONS + 1];
    static int[] pivotCol = new int[MAX_COUNTERS];
    static int[] isPivot = new int[MAX_BUTTONS];
    static int[] pivotRows = new int[MAX_BUTTONS];
    static int[] freeVars = new int[MAX_BUTTONS];
    static int numFree;
    static int[] maxPresses = new int[MAX_BUTTONS];
    static int[] freeValues = new int[MAX_BUTTONS];
    static int bestResult;

    static void ParseLine(string line)
    {
        numCounters = 0;
        numButtons = 0;
        int i = 0;
        while (i < line.Length)
        {
            if (line[i] == '(')
            {
                i++;
                int btnIdx = numButtons++;
                btnSize[btnIdx] = 0;
                while (i < line.Length && line[i] != ')')
                {
                    int x = 0;
                    while (i < line.Length && line[i] >= '0' && line[i] <= '9')
                        x = x * 10 + (line[i++] - '0');
                    buttons[btnIdx, btnSize[btnIdx]++] = x;
                    if (i < line.Length && line[i] == ',') i++;
                }
                if (i < line.Length && line[i] == ')') i++;
            }
            else if (line[i] == '{')
            {
                i++;
                while (i < line.Length && line[i] != '}')
                {
                    int x = 0;
                    while (i < line.Length && line[i] >= '0' && line[i] <= '9')
                        x = x * 10 + (line[i++] - '0');
                    targets[numCounters++] = x;
                    if (i < line.Length && line[i] == ',') i++;
                }
                break;
            }
            else i++;
        }
    }

    static void Gauss()
    {
        for (int j = 0; j < numCounters; j++)
        {
            for (int i = 0; i <= numButtons; i++) matrix[j, i] = 0;
            matrix[j, numButtons] = targets[j];
        }
        for (int i = 0; i < numButtons; i++)
            for (int j = 0; j < btnSize[i]; j++)
            {
                int c = buttons[i, j];
                if (c < numCounters) matrix[c, i] = 1;
            }

        for (int i = 0; i < numCounters; i++) pivotCol[i] = -1;
        int row = 0;
        for (int col = 0; col < numButtons && row < numCounters; col++)
        {
            int maxRow = row;
            for (int r = row + 1; r < numCounters; r++)
                if (Math.Abs(matrix[r, col]) > Math.Abs(matrix[maxRow, col])) maxRow = r;
            if (Math.Abs(matrix[maxRow, col]) < 1e-9) continue;
            for (int c = 0; c <= numButtons; c++)
            {
                double tmp = matrix[row, c];
                matrix[row, c] = matrix[maxRow, c];
                matrix[maxRow, c] = tmp;
            }
            double scale = matrix[row, col];
            for (int c = col; c <= numButtons; c++) matrix[row, c] /= scale;
            for (int r = 0; r < numCounters; r++)
                if (r != row && Math.Abs(matrix[r, col]) > 1e-9)
                {
                    double factor = matrix[r, col];
                    for (int c = col; c <= numButtons; c++) matrix[r, c] -= factor * matrix[row, c];
                }
            pivotCol[row] = col;
            row++;
        }
        int rank = row;
        for (int i = 0; i < numButtons; i++) { isPivot[i] = 0; pivotRows[i] = -1; }
        for (int r = 0; r < rank; r++)
        {
            int c = pivotCol[r];
            if (c >= 0) { isPivot[c] = 1; pivotRows[c] = r; }
        }
        numFree = 0;
        for (int i = 0; i < numButtons; i++) if (isPivot[i] == 0) freeVars[numFree++] = i;
        for (int i = 0; i < numButtons; i++)
        {
            int m = INF;
            for (int j = 0; j < btnSize[i]; j++)
            {
                int c = buttons[i, j];
                if (c < numCounters && targets[c] < m) m = targets[c];
            }
            maxPresses[i] = m == INF ? 0 : m;
        }
        for (int i = 0; i < numFree; i++)
            for (int j = i + 1; j < numFree; j++)
                if (maxPresses[freeVars[i]] > maxPresses[freeVars[j]])
                {
                    int t = freeVars[i];
                    freeVars[i] = freeVars[j];
                    freeVars[j] = t;
                }
    }

    static int ComputePivots(int[] presses)
    {
        Array.Clear(presses, 0, numButtons);
        for (int i = 0; i < numFree; i++) presses[freeVars[i]] = freeValues[i];
        for (int r = numCounters - 1; r >= 0; r--)
        {
            int col = pivotCol[r];
            if (col < 0) continue;
            double val = matrix[r, numButtons];
            for (int c = col + 1; c < numButtons; c++) val -= matrix[r, c] * presses[c];
            int intVal = (int)Math.Round(val);
            if (Math.Abs(val - intVal) > 1e-6) return 0;
            if (intVal < 0 || intVal > maxPresses[col]) return 0;
            presses[col] = intVal;
        }
        int sum = 0;
        for (int i = 0; i < numButtons; i++) sum += presses[i];
        return sum;
    }

    static void Enumerate(int idx, int curSum)
    {
        if (curSum >= bestResult) return;
        if (idx == numFree)
        {
            int[] presses = new int[MAX_BUTTONS];
            int sum = ComputePivots(presses);
            if (sum > 0 && sum < bestResult) bestResult = sum;
            return;
        }
        int fv = freeVars[idx];
        int maxVal = maxPresses[fv];
        for (int v = 0; v <= maxVal; v++)
        {
            freeValues[idx] = v;
            Enumerate(idx + 1, curSum + v);
        }
    }

    static int Solve()
    {
        Gauss();
        bestResult = INF;
        Enumerate(0, 0);
        return bestResult == INF ? -1 : bestResult;
    }

    static void Main()
    {
        var lines = File.ReadAllLines("input.txt");
        int total = 0;
        foreach (var raw in lines)
        {
            var line = raw.Trim();
            if (line.Length == 0) continue;
            ParseLine(line);
            int res = Solve();
            if (res > 0) total += res;
        }
        Console.WriteLine(total);
    }
}
