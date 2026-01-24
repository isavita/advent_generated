using System;
using System.IO;
using System.Collections.Generic;
using System.Linq;
using System.Text;

class Program{
    static void Main(){
        var lines=File.ReadAllLines("input.txt");
        string init="";
        var rules=new Dictionary<string,bool>();
        foreach(var line in lines){
            if(line.StartsWith("initial state:"))init=line.Substring(15).Trim();
            else if(line.Contains("=>")){
                var parts=line.Split(new[]{"=>"},StringSplitOptions.RemoveEmptyEntries);
                rules[parts[0].Trim()]=parts[1].Trim()=="#";
            }
        }
        var plants=new HashSet<int>();
        for(int i=0;i<init.Length;i++)if(init[i]=='#')plants.Add(i);
        string prevPattern=null;
        long prevSum=0;
        const long target=50000000000;
        for(long gen=0;gen<target;gen++){
            int min=plants.Min();
            int max=plants.Max();
            var next=new HashSet<int>();
            for(int i=min-2;i<=max+2;i++){
                var sb=new StringBuilder(5);
                for(int j=i-2;j<=i+2;j++)sb.Append(plants.Contains(j)?'#':'.');
                var pat=sb.ToString();
                if(rules.TryGetValue(pat,out bool grow)&&grow)next.Add(i);
            }
            plants=next;
            min=plants.Min();
            max=plants.Max();
            var curPat=new StringBuilder(max-min+1);
            for(int i=min;i<=max;i++)curPat.Append(plants.Contains(i)?'#':'.');
            var curPattern=curPat.ToString();
            long curSum=plants.Sum(p=>(long)p);
            if(prevPattern!=null&&curPattern==prevPattern){
                long offset=curSum-prevSum;
                long remaining=target-gen-1;
                Console.WriteLine(curSum+offset*remaining);
                return;
            }
            prevPattern=curPattern;
            prevSum=curSum;
        }
        Console.WriteLine(plants.Sum(p=>(long)p));
    }
}