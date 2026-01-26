
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

public class JurassicJigsaw
{
    public class Tile
    {
        public int Id { get; set; }
        public char[][] Data { get; set; }
        public string[] Borders { get; set; }

        public Tile(int id, char[][] data)
        {
            Id = id;
            Data = data;
            Borders = CalculateBorders();
        }

        private string[] CalculateBorders()
        {
            string[] borders = new string[8];
            int n = Data.Length;

            string top = new string(Data[0]);
            string bottom = new string(Data[n - 1]);
            string left = string.Concat(Enumerable.Range(0, n).Select(i => Data[i][0]));
            string right = string.Concat(Enumerable.Range(0, n).Select(i => Data[i][n - 1]));

            borders[0] = top;
            borders[1] = right;
            borders[2] = bottom;
            borders[3] = left;
            borders[4] = new string(top.Reverse().ToArray());
            borders[5] = new string(right.Reverse().ToArray());
            borders[6] = new string(bottom.Reverse().ToArray());
            borders[7] = new string(left.Reverse().ToArray());

            return borders;
        }
    }

    public static void Main(string[] args)
    {
        List<Tile> tiles = ReadTiles("input.txt");
        Dictionary<string, List<Tile>> borderMap = BuildBorderMap(tiles);
        long cornerProduct = FindCornerProduct(tiles, borderMap);

        Console.WriteLine(cornerProduct);
    }

    private static List<Tile> ReadTiles(string filename)
    {
        List<Tile> tiles = new List<Tile>();
        using (StreamReader sr = File.OpenText(filename))
        {
            string line;
            int? id = null;
            List<string> tileData = new List<string>();

            while ((line = sr.ReadLine()) != null)
            {
                if (line.StartsWith("Tile"))
                {
                    id = int.Parse(line.Substring(5, line.Length - 6).Trim());
                }
                else if (string.IsNullOrEmpty(line))
                {
                    if (id.HasValue && tileData.Count > 0)
                    {
                        tiles.Add(CreateTile(id.Value, tileData));
                        tileData.Clear();
                    }
                }
                else
                {
                    tileData.Add(line);
                }
            }

            if (id.HasValue && tileData.Count > 0)
            {
                tiles.Add(CreateTile(id.Value, tileData));
            }
        }

        return tiles;
    }

    private static Tile CreateTile(int id, List<string> tileData)
    {
        int n = tileData.Count;
        char[][] data = tileData.Select(s => s.ToCharArray()).ToArray();
        return new Tile(id, data);
    }

    private static Dictionary<string, List<Tile>> BuildBorderMap(List<Tile> tiles)
    {
        Dictionary<string, List<Tile>> borderMap = new Dictionary<string, List<Tile>>();

        foreach (Tile tile in tiles)
        {
            foreach (string border in tile.Borders)
            {
                if (!borderMap.TryGetValue(border, out List<Tile> list))
                {
                    list = new List<Tile>();
                    borderMap[border] = list;
                }

                list.Add(tile);
            }
        }

        return borderMap;
    }

    private static long FindCornerProduct(List<Tile> tiles, Dictionary<string, List<Tile>> borderMap)
    {
        long product = 1;

        foreach (Tile tile in tiles)
        {
            int uniqueBorders = tile.Borders.Take(4).Count(border => borderMap[border].Count == 1);

            if (uniqueBorders == 2)
            {
                product *= tile.Id;
            }
        }

        return product;
    }
}
