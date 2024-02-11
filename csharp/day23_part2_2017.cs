
using System;
using System.IO;

class Program
{
    static bool IsPrime(int n)
    {
        for (int i = 2; i * i <= n; i++)
        {
            if (n % i == 0)
            {
                return false;
            }
        }
        return true;
    }

    static void Main()
    {
        int b = 57 * 100 + 100000;
        int c = b + 17000;
        int h = 0;

        for (int x = b; x <= c; x += 17)
        {
            if (!IsPrime(x))
            {
                h++;
            }
        }

        Console.WriteLine(h);
    }
}
