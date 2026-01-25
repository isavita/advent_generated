using System;
using System.IO;

class Program
{
    const int NUM_STEPS = 2000;
    const int PATTERN_COUNT = 130321; // 19^4
    const uint MOD_MASK = (1u << 24) - 1;
    static uint NextSecret(uint s)
    {
        uint x = s * 64;
        s ^= x;
        s &= MOD_MASK;
        x = s / 32;
        s ^= x;
        s &= MOD_MASK;
        x = s * 2048;
        s ^= x;
        s &= MOD_MASK;
        return s;
    }
    static int Encode(int c1, int c2, int c3, int c4)
    {
        return (c1 + 9) + (c2 + 9) * 19 + (c3 + 9) * 361 + (c4 + 9) * 6859;
    }
    static void Main()
    {
        var lines = File.ReadAllLines("input.txt");
        var seeds = new int[lines.Length];
        for (int i = 0; i < lines.Length; i++) seeds[i] = int.Parse(lines[i]);

        if (seeds.Length == 0) { Console.WriteLine(0); return; }

        var globalSum = new long[PATTERN_COUNT];
        var prices = new int[NUM_STEPS + 1];
        var changes = new int[NUM_STEPS];
        var localPrice = new int[PATTERN_COUNT];

        foreach (var seed in seeds)
        {
            uint s = (uint)seed;
            for (int i = 0; i <= NUM_STEPS; i++)
            {
                prices[i] = (int)(s % 10);
                s = NextSecret(s);
            }
            for (int i = 0; i < NUM_STEPS; i++) changes[i] = prices[i + 1] - prices[i];
            Array.Fill(localPrice, -1);
            for (int i = 0; i <= NUM_STEPS - 4; i++)
            {
                int c1 = changes[i], c2 = changes[i + 1], c3 = changes[i + 2], c4 = changes[i + 3];
                if (Math.Abs(c1) <= 9 && Math.Abs(c2) <= 9 && Math.Abs(c3) <= 9 && Math.Abs(c4) <= 9)
                {
                    int idx = Encode(c1, c2, c3, c4);
                    if (localPrice[idx] < 0) localPrice[idx] = prices[i + 4];
                }
            }
            for (int i = 0; i < PATTERN_COUNT; i++)
                if (localPrice[i] >= 0) globalSum[i] += localPrice[i];
        }

        long max = 0;
        foreach (var v in globalSum) if (v > max) max = v;
        Console.WriteLine(max);
    }
}