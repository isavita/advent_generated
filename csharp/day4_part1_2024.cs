
using System;
using System.IO;

class Program
{
    static readonly (int dx, int dy)[] dirs =
    {
        (0,1),(1,0),(1,1),(-1,1),
        (0,-1),(-1,0),(-1,-1),(1,-1)
    };

    static bool CheckWord(char[][] g, string w, int x, int y, (int dx,int dy) d)
    {
        int h=g.Length, l=g[0].Length;
        for(int i=0;i<w.Length;i++)
        {
            int nx=x+d.dx*i, ny=y+d.dy*i;
            if((uint)nx>=(uint)h||(uint)ny>=(uint)l||g[nx][ny]!=w[i])
                return false;
        }
        return true;
    }

    static int Count(char[][] g, string w)
    {
        int c=0,h=g.Length,l=g[0].Length;
        for(int i=0;i<h;i++)
            for(int j=0;j<l;j++)
                foreach(var d in dirs)
                    if(CheckWord(g,w,i,j,d)) c++;
        return c;
    }

    static void Main()
    {
        var g=File.ReadAllLines("input.txt");
        int rows=g.Length,cols=g[0].Length;
        var grid=new char[rows][];
        for(int i=0;i<rows;i++) grid[i]=g[i].ToCharArray();
        Console.WriteLine(Count(grid,"XMAS"));
    }
}
