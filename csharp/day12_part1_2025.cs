
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

class Program
{
    struct Point { public int r, c; }
    struct Piece { public List<Point> p; }

    static Piece Normalize(Piece inP)
    {
        if (inP.p.Count == 0) return new Piece { p = new List<Point>() };
        int minR = inP.p.Min(pt => pt.r);
        int minC = inP.p.Min(pt => pt.c);
        var outP = new Piece { p = new List<Point>() };
        foreach (var pt in inP.p)
            outP.p.Add(new Point { r = pt.r - minR, c = pt.c - minC });
        outP.p = outP.p.OrderBy(pt => pt.r).ThenBy(pt => pt.c).ToList();
        return outP;
    }

    static Piece RotateP(Piece a)
    {
        var b = new Piece { p = new List<Point>() };
        foreach (var pt in a.p)
            b.p.Add(new Point { r = pt.c, c = -pt.r });
        return b;
    }

    static Piece FlipP(Piece a)
    {
        var b = new Piece { p = new List<Point>() };
        foreach (var pt in a.p)
            b.p.Add(new Point { r = pt.r, c = -pt.c });
        return b;
    }

    static bool PieceEqual(Piece a, Piece b)
    {
        if (a.p.Count != b.p.Count) return false;
        for (int i = 0; i < a.p.Count; i++)
            if (a.p[i].r != b.p[i].r || a.p[i].c != b.p[i].c) return false;
        return true;
    }

    static List<Piece> GenerateVariations(Piece baseP)
    {
        var uniq = new List<Piece>();
        Piece curr = baseP;
        for (int i = 0; i < 4; i++)
        {
            var n = Normalize(curr);
            if (!uniq.Any(u => PieceEqual(u, n))) uniq.Add(n);
            var f = FlipP(curr);
            var nf = Normalize(f);
            if (!uniq.Any(u => PieceEqual(u, nf))) uniq.Add(nf);
            var r = RotateP(curr);
            curr = r;
        }
        return uniq;
    }

    static bool CanPlace(int rows, int cols, bool[] grid, Piece p, int rr, int cc)
    {
        foreach (var pt in p.p)
        {
            int nr = rr + pt.r, nc = cc + pt.c;
            if (nr < 0 || nr >= rows || nc < 0 || nc >= cols) return false;
            if (grid[nr * cols + nc]) return false;
        }
        return true;
    }

    static void Place(int cols, bool[] grid, Piece p, int rr, int cc, bool val)
    {
        foreach (var pt in p.p)
            grid[(rr + pt.r) * cols + (cc + pt.c)] = val;
    }

    static bool CheckIslands(int rows, int cols, bool[] grid, int[] counts, int slackIdx, Piece[] shapes)
    {
        int minReal = int.MaxValue; bool hasReal = false;
        for (int i = 0; i < counts.Length; i++)
            if (i != slackIdx && counts[i] > 0) { if (shapes[i].p.Count < minReal) minReal = shapes[i].p.Count; hasReal = true; }
        if (!hasReal) return true;
        int availSlack = counts[slackIdx];
        bool[] vis = new bool[rows * cols];
        int[] q = new int[rows * cols];
        for (int i = 0; i < rows * cols; i++)
        {
            if (!grid[i] && !vis[i])
            {
                int qs = 0, qe = 0;
                q[qe++] = i; vis[i] = true;
                int size = 0;
                while (qs < qe)
                {
                    int cur = q[qs++]; size++;
                    int r = cur / cols, c = cur % cols;
                    if (r > 0) { int n = (r - 1) * cols + c; if (!grid[n] && !vis[n]) { vis[n] = true; q[qe++] = n; } }
                    if (r < rows - 1) { int n = (r + 1) * cols + c; if (!grid[n] && !vis[n]) { vis[n] = true; q[qe++] = n; } }
                    if (c > 0) { int n = r * cols + (c - 1); if (!grid[n] && !vis[n]) { vis[n] = true; q[qe++] = n; } }
                    if (c < cols - 1) { int n = r * cols + (c + 1); if (!grid[n] && !vis[n]) { vis[n] = true; q[qe++] = n; } }
                }
                if (size < minReal)
                {
                    if (availSlack >= size) availSlack -= size;
                    else return false;
                }
            }
        }
        return true;
    }

    static bool SolveRec(int rows, int cols, bool[] grid, int[] counts, int[] ids, List<Piece>[] variations, int slackIdx, Piece[] shapes)
    {
        int empty = -1;
        for (int i = 0; i < rows * cols; i++)
            if (!grid[i]) { empty = i; break; }
        if (empty == -1) return true;
        int r = empty / cols, c = empty % cols;
        if (!CheckIslands(rows, cols, grid, counts, slackIdx, shapes)) return false;
        foreach (int id in ids)
        {
            if (counts[id] == 0) continue;
            counts[id]--;
            foreach (var p in variations[id])
            {
                if (CanPlace(rows, cols, grid, p, r, c))
                {
                    Place(cols, grid, p, r, c, true);
                    if (SolveRec(rows, cols, grid, counts, ids, variations, slackIdx, shapes)) return true;
                    Place(cols, grid, p, r, c, false);
                }
            }
            counts[id]++;
        }
        return false;
    }

    static void Main()
    {
        var lines = File.ReadAllLines("input.txt").Select(l => l.Trim()).Where(l => l.Length > 0).ToList();
        int maxId = -1000000;
        foreach (var l in lines)
        {
            if (l.EndsWith(":"))
            {
                int id = int.Parse(l.Substring(0, l.Length - 1));
                if (id > maxId) maxId = id;
            }
        }
        if (maxId < 0) maxId = -1;
        int arrSize = maxId + 2;
        int slackIdx = maxId + 1;
        var shapes = new Piece[arrSize];
        for (int i = 0; i < arrSize; i++) shapes[i] = new Piece { p = new List<Point>() };
        bool parsingShapes = true;
        int currentID = -1;
        var currentShapeLines = new List<string>();
        var regionLines = new List<string>();
        foreach (var raw in lines)
        {
            var s = raw.Trim();
            if (s.Length == 0) continue;
            if (s.Contains('x') && s.Contains(':')) parsingShapes = false;
            if (parsingShapes)
            {
                if (s.EndsWith(":"))
                {
                    if (currentID != -1 && currentShapeLines.Count > 0)
                    {
                        var p = new Piece { p = new List<Point>() };
                        for (int rr = 0; rr < currentShapeLines.Count; rr++)
                            for (int cc = 0; cc < currentShapeLines[rr].Length; cc++)
                                if (currentShapeLines[rr][cc] == '#')
                                    p.p.Add(new Point { r = rr, c = cc });
                        shapes[currentID] = Normalize(p);
                    }
                    currentID = int.Parse(s.Substring(0, s.Length - 1));
                    currentShapeLines.Clear();
                }
                else currentShapeLines.Add(s);
            }
            else regionLines.Add(s);
        }
        if (currentID != -1 && currentShapeLines.Count > 0)
        {
            var p = new Piece { p = new List<Point>() };
            for (int rr = 0; rr < currentShapeLines.Count; rr++)
                for (int cc = 0; cc < currentShapeLines[rr].Length; cc++)
                    if (currentShapeLines[rr][cc] == '#')
                        p.p.Add(new Point { r = rr, c = cc });
            shapes[currentID] = Normalize(p);
        }
        for (int i = 0; i < arrSize; i++) if (shapes[i].p.Count == 0) shapes[i] = new Piece { p = new List<Point>() };
        shapes[slackIdx].p.Add(new Point { r = 0, c = 0 });
        var variations = new List<Piece>[arrSize];
        for (int i = 0; i < arrSize; i++)
            variations[i] = shapes[i].p.Count == 0 ? new List<Piece>() : GenerateVariations(shapes[i]);
        int solvedCount = 0;
        foreach (var lnstr in regionLines)
        {
            var parts = lnstr.Split(':');
            if (parts.Length != 2) continue;
            var dims = parts[0].Trim();
            var countsStr = parts[1].Trim();
            var wxh = dims.Split('x');
            if (wxh.Length != 2) continue;
            int w = int.Parse(wxh[0]), h = int.Parse(wxh[1]);
            int gridSize = w * h;
            var pieceCounts = new int[arrSize];
            int totalArea = 0;
            var toks = countsStr.Split(new[] { ' ', '\t' }, StringSplitOptions.RemoveEmptyEntries);
            int idx = 0;
            foreach (var t in toks)
            {
                int c = int.Parse(t);
                if (c > 0 && idx < arrSize - 1)
                {
                    pieceCounts[idx] = c;
                    totalArea += c * shapes[idx].p.Count;
                }
                idx++;
            }
            if (totalArea > gridSize) continue;
            int slack = gridSize - totalArea;
            if (slack > 0) pieceCounts[slackIdx] = slack;
            var ids = Enumerable.Range(0, arrSize).Where(i => pieceCounts[i] > 0).OrderByDescending(i => shapes[i].p.Count).ToArray();
            var grid = new bool[gridSize];
            if (SolveRec(h, w, grid, pieceCounts, ids, variations, slackIdx, shapes)) solvedCount++;
        }
        Console.WriteLine("Number of regions that fit all presents: " + solvedCount);
    }
}
