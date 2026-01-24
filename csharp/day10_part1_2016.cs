
using System;
using System.IO;
using System.Linq;

struct Bot
{
    public int low_type;
    public int low_id;
    public int high_type;
    public int high_id;
    public int[] chips;
    public int chip_count;
}

class Program
{
    const int MAX_BOD = 256;
    const int MAX_OUT = 256;

    static void Main()
    {
        var bots = new Bot[MAX_BOD];
        for (int i = 0; i < MAX_BOD; i++)
            bots[i].chips = new int[2];

        var output = new int[MAX_OUT];

        foreach (var line in File.ReadAllLines("input.txt"))
        {
            if (line.StartsWith("value"))
            {
                var p = line.Split(' ');
                int v = int.Parse(p[1]);
                int b = int.Parse(p.Last());
                bots[b].chips[bots[b].chip_count++] = v;
            }
            else
            {
                var p = line.Split(' ');
                int b = int.Parse(p[1]);
                bots[b].low_type = p[5] == "output" ? 1 : 0;
                bots[b].low_id = int.Parse(p[6]);
                bots[b].high_type = p[10] == "output" ? 1 : 0;
                bots[b].high_id = int.Parse(p[11]);
            }
        }

        int target = -1;
        while (true)
        {
            bool moved = false;
            for (int i = 0; i < MAX_BOD; i++)
            {
                if (bots[i].chip_count == 2)
                {
                    moved = true;
                    int lo = Math.Min(bots[i].chips[0], bots[i].chips[1]);
                    int hi = Math.Max(bots[i].chips[0], bots[i].chips[1]);

                    if (lo == 17 && hi == 61) target = i;

                    if (bots[i].low_type == 0)
                    {
                        bots[bots[i].low_id].chips[bots[bots[i].low_id].chip_count++] = lo;
                    }
                    else
                    {
                        output[bots[i].low_id] = lo;
                    }

                    if (bots[i].high_type == 0)
                    {
                        bots[bots[i].high_id].chips[bots[bots[i].high_id].chip_count++] = hi;
                    }
                    else
                    {
                        output[bots[i].high_id] = hi;
                    }

                    bots[i].chip_count = 0;
                }
            }
            if (!moved) break;
        }

        Console.WriteLine(target);
    }
}
