
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");
        long genAStart = long.Parse(lines[0]);
        long genBStart = long.Parse(lines[1]);

        long genAFactor = 16807;
        long genBFactor = 48271;
        long modulus = 2147483647;

        long genA = genAStart;
        long genB = genBStart;
        int matches = 0;

        for (int i = 0; i < 5000000; i++)
        {
            // Generate next value for A that is a multiple of 4
            do
            {
                genA = (genA * genAFactor) % modulus;
            } while (genA % 4 != 0);

            // Generate next value for B that is a multiple of 8
            do
            {
                genB = (genB * genBFactor) % modulus;
            } while (genB % 8 != 0);

            if ((genA & 0xFFFF) == (genB & 0xFFFF))
            {
                matches++;
            }
        }

        Console.WriteLine(matches);
    }
}
