using System;
using System.IO;

class Program
{
    static readonly long DeckSize = 119315717514047L;
    static readonly long ShuffleTimes = 101741582076661L;

    static long Mod(long x, long m)
    {
        return ((x % m) + m) % m;
    }

    // Modular multiplication using 128-bit arithmetic via decimal
    static long ModMul(long a, long b, long mod)
    {
        a = Mod(a, mod);
        b = Mod(b, mod);
        
        // Use decimal for intermediate calculation (has ~28 digits precision)
        decimal da = a;
        decimal db = b;
        decimal dm = mod;
        decimal product = da * db;
        decimal quotient = Math.Floor(product / dm);
        decimal remainder = product - quotient * dm;
        
        long result = (long)remainder;
        return Mod(result, mod);
    }

    static long ModPow(long baseVal, long exp, long mod)
    {
        long result = 1;
        baseVal = Mod(baseVal, mod);
        while (exp > 0)
        {
            if ((exp & 1) == 1)
                result = ModMul(result, baseVal, mod);
            baseVal = ModMul(baseVal, baseVal, mod);
            exp >>= 1;
        }
        return result;
    }

    static long ModInverse(long a, long mod)
    {
        // Using Fermat's little theorem: a^(-1) = a^(p-2) mod p (for prime p)
        return ModPow(a, mod - 2, mod);
    }

    static (long a, long b) ComposeTransforms(long a1, long b1, long a2, long b2)
    {
        // Applying f1 then f2: f2(f1(x)) = a2*(a1*x + b1) + b2 = (a2*a1)*x + (a2*b1 + b2)
        long newA = ModMul(a2, a1, DeckSize);
        long newB = Mod(ModMul(a2, b1, DeckSize) + b2, DeckSize);
        return (newA, newB);
    }

    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");

        // Start with identity transformation: f(x) = 1*x + 0
        long a = 1, b = 0;

        foreach (string line in lines)
        {
            string trimmed = line.Trim();
            if (string.IsNullOrEmpty(trimmed)) continue;

            long newA, newB;

            if (trimmed == "deal into new stack")
            {
                // Reverses: new_pos = deck_size - 1 - old_pos = -1 * old_pos + (deck_size - 1)
                newA = -1;
                newB = DeckSize - 1;
            }
            else if (trimmed.StartsWith("cut "))
            {
                // Cut N: new_pos = (old_pos - N) mod deck_size
                long n = long.Parse(trimmed.Substring(4));
                newA = 1;
                newB = -n;
            }
            else if (trimmed.StartsWith("deal with increment "))
            {
                // Deal with increment N: new_pos = (old_pos * N) mod deck_size
                long n = long.Parse(trimmed.Substring(20));
                newA = n;
                newB = 0;
            }
            else
            {
                throw new Exception("Unknown operation: " + trimmed);
            }

            // Compose: apply current transformation, then the new one
            (a, b) = ComposeTransforms(a, b, newA, newB);
        }

        // Now (a, b) represents one complete shuffle: new_pos = a * old_pos + b
        // We need to apply it ShuffleTimes times
        // 
        // For k applications of f(x) = ax + b:
        // f^k(x) = a^k * x + b * (1 + a + a^2 + ... + a^(k-1))
        //        = a^k * x + b * (a^k - 1) / (a - 1)

        long ak = ModPow(a, ShuffleTimes, DeckSize);

        // b_k = b * (a^k - 1) * inv(a - 1)
        long numerator = Mod(ak - 1, DeckSize);
        long denominator = Mod(a - 1, DeckSize);
        long bk = ModMul(ModMul(b, numerator, DeckSize), ModInverse(denominator, DeckSize), DeckSize);

        // After k shuffles: final_pos = ak * initial_pos + bk
        // We want to find initial_pos given final_pos = 2020
        // initial_pos = (final_pos - bk) * inv(ak)
        // Since cards start in factory order (card i at position i),
        // the card number equals its initial position

        long finalPos = 2020;
        long initialPos = ModMul(Mod(finalPos - bk, DeckSize), ModInverse(ak, DeckSize), DeckSize);

        Console.WriteLine(initialPos);
    }
}
