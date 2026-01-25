using System;
using System.IO;

struct Point
{
    public int X, Y, Z, T;
    public Point(int x,int y,int z,int t){X=x;Y=y;Z=z;T=t;}
}

class UnionFind
{
    int[] parent, rank;
    public UnionFind(int n){parent=new int[n];rank=new int[n];for(int i=0;i<n;i++)parent[i]=i;}
    public int Find(int x){while(x!=parent[x]){parent[x]=parent[parent[x]];x=parent[x];}return x;}
    public void Union(int a,int b){
        int ra=Find(a), rb=Find(b);
        if(ra==rb)return;
        if(rank[ra]<rank[rb])parent[ra]=rb;
        else if(rank[ra]>rank[rb])parent[rb]=ra;
        else{parent[rb]=ra;rank[ra]++;}
    }
    public int CountRoots(){
        int cnt=0;
        for(int i=0;i<parent.Length;i++)if(i==parent[i])cnt++;
        return cnt;
    }
}

class Program
{
    static int Manhattan(Point a, Point b)=>Math.Abs(a.X-b.X)+Math.Abs(a.Y-b.Y)+Math.Abs(a.Z-b.Z)+Math.Abs(a.T-b.T);
    static void Main(){
        var lines=File.ReadAllLines("input.txt");
        var points=new Point[lines.Length];
        for(int i=0;i<lines.Length;i++){
            var s=lines[i].Split(',');
            points[i]=new Point(int.Parse(s[0]),int.Parse(s[1]),int.Parse(s[2]),int.Parse(s[3]));
        }
        var uf=new UnionFind(points.Length);
        for(int i=0;i<points.Length;i++){
            for(int j=i+1;j<points.Length;j++){
                if(Manhattan(points[i],points[j])<=3)uf.Union(i,j);
            }
        }
        Console.WriteLine(uf.CountRoots());
    }
}