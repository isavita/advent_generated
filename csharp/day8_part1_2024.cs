using System;
using System.IO;
using System.Collections.Generic;

struct Point { public int Y; public int X; public Point(int y,int x){Y=y;X=x;} }

class PointComparer : IEqualityComparer<Point>
{
    public static readonly PointComparer Instance = new PointComparer();
    public bool Equals(Point a, Point b) => a.Y==b.Y && a.X==b.X;
    public int GetHashCode(Point p) => (p.Y*1001)+p.X;
}

class Program
{
    static void Main()
    {
        var lines = File.ReadAllLines("input.txt");
        int h = lines.Length;
        int w = lines[0].Length;
        List<Point>[] antennas = new List<Point>[256];
        for(int y=0;y<h;y++){
            string line=lines[y];
            for(int x=0;x<w;x++){
                char c=line[x];
                if(c!='.'){
                    var list=antennas[(int)c];
                    if(list==null){list=new List<Point>(); antennas[(int)c]=list;}
                    list.Add(new Point(y,x));
                }
            }
        }
        var antinodes=new HashSet<Point>(PointComparer.Instance);
        for(int i=0;i<256;i++){
            var list=antennas[i];
            if(list==null)continue;
            int n=list.Count;
            for(int j=0;j<n;j++){
                var A=list[j];
                for(int k=j+1;k<n;k++){
                    var B=list[k];
                    int py = 2*A.Y - B.Y;
                    int px = 2*A.X - B.X;
                    if(py>=0 && py<h && px>=0 && px<w) antinodes.Add(new Point(py,px));
                    py = 2*B.Y - A.Y;
                    px = 2*B.X - A.X;
                    if(py>=0 && py<h && px>=0 && px<w) antinodes.Add(new Point(py,px));
                }
            }
        }
        Console.WriteLine(antinodes.Count);
    }
}
