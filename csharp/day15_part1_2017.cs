using System;
using System.IO;

class Program
{
    static void Main()
    {
        using (StreamReader sr = new StreamReader("input.txt"))
        {
            int genAStart = int.Parse(sr.ReadLine());
            int genBStart = int.Parse(sr.ReadLine());

            int genAFactor = 16807;
            int genBFactor = 48271;
            int modulus = 2147483647;

            long genA = genAStart;
            long genB = genBStart;
            int matches = 0;

            for (int i = 0; i < 40000000; i++)
            {
                genA = (genA * genAFactor) % modulus;
                genB = (genB * genBFactor) % modulus;

                if ((genA & 0xFFFF) == (genB & 0xFFFF))
                {
                    matches++;
                }
            }

            Console.WriteLine(matches);
        }
    }
}