
using System;
using System.IO;

class Program {
    static void Main() {
        string[] data = File.ReadAllLines("input.txt");
        long card = long.Parse(data[0]), door = long.Parse(data[1]), v = 1, l = 0;
        while (v != card) { v = v * 7 % 20201227; l++; }
        v = 1;
        for (long i = 0; i < l; i++) v = v * door % 20201227;
        Console.WriteLine(v);
    }
}
