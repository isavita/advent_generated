
using System;
using System.IO;
using System.Collections.Generic;

class Claim
{
    public int id;
    public int left;
    public int top;
    public int width;
    public int height;
}

class Program
{
    static Claim ParseClaim(string s)
    {
        Claim c = new Claim();
        string[] parts = s.Split(new char[] { '#', '@', ',', ':', 'x', ' ' }, StringSplitOptions.RemoveEmptyEntries);
        c.id = int.Parse(parts[0]);
        c.left = int.Parse(parts[1]);
        c.top = int.Parse(parts[2]);
        c.width = int.Parse(parts[3]);
        c.height = int.Parse(parts[4]);
        return c;
    }

    static List<Claim> ReadClaims(string filename)
    {
        List<Claim> claims = new List<Claim>();
        using (StreamReader sr = new StreamReader(filename))
        {
            string line;
            while ((line = sr.ReadLine()) != null)
            {
                Claim claim = ParseClaim(line);
                claims.Add(claim);
            }
        }
        return claims;
    }

    static int CountOverlappingInches(List<Claim> claims)
    {
        Dictionary<string, int> fabric = new Dictionary<string, int>();
        foreach (Claim claim in claims)
        {
            for (int i = claim.left; i < claim.left + claim.width; i++)
            {
                for (int j = claim.top; j < claim.top + claim.height; j++)
                {
                    string coord = i + "," + j;
                    if (fabric.ContainsKey(coord))
                    {
                        fabric[coord]++;
                    }
                    else
                    {
                        fabric[coord] = 1;
                    }
                }
            }
        }

        int overlapping = 0;
        foreach (int count in fabric.Values)
        {
            if (count > 1)
            {
                overlapping++;
            }
        }
        return overlapping;
    }

    static void Main()
    {
        List<Claim> claims = ReadClaims("input.txt");
        int overlapping = CountOverlappingInches(claims);
        Console.WriteLine(overlapping);
    }
}
