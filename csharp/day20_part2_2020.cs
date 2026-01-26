
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

public class Tile
{
    public int Id { get; set; }
    public List<List<char>> Contents { get; set; }

    public Tile(int id, List<List<char>> contents)
    {
        Id = id;
        Contents = contents;
    }
}

public class Program
{
    private static readonly string MONSTER = @"
                  # 
#    ##    ##    ###
 #  #  #  #  #  #   ";

    public static void Main(string[] args)
    {
        var input = File.ReadAllText("input.txt").Trim();
        Console.WriteLine(Solve(input));
    }

    public static int Solve(string input)
    {
        var tiles = ParseTilesFromInput(input);
        var edgeSize = (int)Math.Sqrt(tiles.Count);
        var assembledTiles = BacktrackAssemble(tiles, new Tile[edgeSize, edgeSize], new bool[tiles.Count]);

        for (var i = 0; i < edgeSize; i++)
        {
            for (var j = 0; j < edgeSize; j++)
            {
                assembledTiles[i, j].Contents = RemoveBordersFromGrid(assembledTiles[i, j].Contents);
            }
        }

        var image = new List<List<char>>();
        for (var bigRow = 0; bigRow < edgeSize; bigRow++)
        {
            for (var subRow = 0; subRow < assembledTiles[0, 0].Contents.Count; subRow++)
            {
                var newRow = new List<char>();
                for (var bigCol = 0; bigCol < edgeSize; bigCol++)
                {
                    newRow.AddRange(assembledTiles[bigRow, bigCol].Contents[subRow]);
                }
                image.Add(newRow);
            }
        }

        var monsterCoords = new List<(int, int)>();
        foreach (var opt in AllGridOrientations(image))
        {
            monsterCoords = FindMonsterCoords(opt);
            if (monsterCoords.Count > 0)
            {
                image = opt;
                break;
            }
        }

        foreach (var coord in monsterCoords)
        {
            image[coord.Item1][coord.Item2] = 'O';
        }

        var roughWatersCount = image.Sum(row => row.Count(c => c == '#'));

        return roughWatersCount;
    }

    public static List<Tile> ParseTilesFromInput(string input)
    {
        return input.Split(new[] { "\r\n\r\n", "\n\n" }, StringSplitOptions.None)
            .Select(block =>
            {
                var lines = block.Split(new[] { "\r\n", "\n" }, StringSplitOptions.None);
                var tileId = int.Parse(lines[0].Substring(5, lines[0].Length - 6));
                var contents = lines.Skip(1).Select(line => line.ToCharArray().ToList()).ToList();
                return new Tile(tileId, contents);
            })
            .ToList();
    }

    public static Tile[,] BacktrackAssemble(List<Tile> tiles, Tile[,] assembledTiles, bool[] usedIndices)
    {
        var edgeSize = (int)Math.Sqrt(tiles.Count);
        for (var row = 0; row < edgeSize; row++)
        {
            for (var col = 0; col < edgeSize; col++)
            {
                if (assembledTiles[row, col] == null)
                {
                    for (var i = 0; i < tiles.Count; i++)
                    {
                        if (!usedIndices[i])
                        {
                            foreach (var opt in AllGridOrientations(tiles[i].Contents))
                            {
                                if (row > 0 && GetRow(opt, true) != GetRow(assembledTiles[row - 1, col].Contents, false)) continue;
                                if (col > 0 && GetCol(opt, true) != GetCol(assembledTiles[row, col - 1].Contents, false)) continue;

                                assembledTiles[row, col] = new Tile(tiles[i].Id, opt);
                                usedIndices[i] = true;

                                if (BacktrackAssemble(tiles, assembledTiles, usedIndices) != null) return assembledTiles;

                                assembledTiles[row, col] = null;
                                usedIndices[i] = false;
                            }
                        }
                    }
                    return null;
                }
            }
        }
        return assembledTiles;
    }

    public static string GetCol(List<List<char>> grid, bool firstCol)
    {
        var str = new char[grid.Count];
        for (var i = 0; i < grid.Count; i++)
        {
            str[i] = firstCol ? grid[i][0] : grid[i][grid[i].Count - 1];
        }
        return new string(str);
    }

    public static string GetRow(List<List<char>> grid, bool firstRow)
    {
        return new string(firstRow ? grid[0].ToArray() : grid[grid.Count - 1].ToArray());
    }

    public static List<List<char>> RemoveBordersFromGrid(List<List<char>> grid)
    {
        return grid.Skip(1).Take(grid.Count - 2).Select(row => row.Skip(1).Take(row.Count - 2).ToList()).ToList();
    }

    public static List<(int, int)> FindMonsterCoords(List<List<char>> image)
    {
        var monsterOffsets = MONSTER.Split(new[] { "\r\n", "\n" }, StringSplitOptions.None)
            .SelectMany((line, r) => line.Select((c, col) => new { c, r, col }))
            .Where(x => x.c == '#')
            .Select(x => (x.r, x.col))
            .ToList();

        var monsterStartingCoords = new List<(int, int)>();
        for (var r = 0; r <= image.Count - monsterOffsets.Max(x => x.Item1) - 1; r++)
        {
            for (var c = 0; c <= image[0].Count - monsterOffsets.Max(x => x.Item2) - 1; c++)
            {
                var monsterFound = true;
                foreach (var offset in monsterOffsets)
                {
                    if (image[r + offset.Item1][c + offset.Item2] != '#')
                    {
                        monsterFound = false;
                        break;
                    }
                }
                if (monsterFound)
                {
                    monsterStartingCoords.Add((r, c));
                }
            }
        }

        return monsterStartingCoords.SelectMany(startingCoord => monsterOffsets.Select(offset => (startingCoord.Item1 + offset.Item1, startingCoord.Item2 + offset.Item2))).ToList();
    }

    public static IEnumerable<List<List<char>>> AllGridOrientations(List<List<char>> grid)
    {
        var orientations = new List<List<List<char>>>();
        orientations.Add(grid);
        for (var i = 0; i < 3; i++)
        {
            orientations.Add(RotateStringGrid(orientations.Last()));
        }
        for (var i = 0; i < 4; i++)
        {
            orientations.Add(MirrorStringGrid(orientations[i]));
        }
        return orientations;
    }

    public static List<List<char>> RotateStringGrid(List<List<char>> grid)
    {
        var n = grid.Count;
        var rotated = Enumerable.Range(0, n).Select(_ => new List<char>(new char[n])).ToList();
        for (var i = 0; i < n; i++)
        {
            for (var j = 0; j < n; j++)
            {
                rotated[n - 1 - j][i] = grid[i][j];
            }
        }
        return rotated;
    }

    public static List<List<char>> MirrorStringGrid(List<List<char>> grid)
    {
        return grid.Select(row => row.AsEnumerable().Reverse().ToList()).ToList();
    }
}
