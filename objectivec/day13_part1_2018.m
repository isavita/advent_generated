
#import <Foundation/Foundation.h>
typedef struct { int x,y,turn; char dir; } Cart;
static Cart mvRight(char t[1000][1000], Cart c){ char n=t[c.y][c.x+1];
    if(n=='\\')c.dir='v';else if(n=='/')c.dir='^';else if(n=='+'){if(!c.turn){c.dir='^';c.turn=1;}else if(c.turn==1)c.turn=2;else{c.dir='v';c.turn=0;}}c.x++;return c;}
static Cart mvLeft (char t[1000][1000], Cart c){ char n=t[c.y][c.x-1];
    if(n=='/')c.dir='v';else if(n=='\\')c.dir='^';else if(n=='+'){if(!c.turn){c.dir='v';c.turn=1;}else if(c.turn==1)c.turn=2;else{c.dir='^';c.turn=0;}}c.x--;return c;}
static Cart mvUp   (char t[1000][1000], Cart c){ char n=t[c.y-1][c.x];
    if(n=='/')c.dir='>';else if(n=='\\')c.dir='<';else if(n=='+'){if(!c.turn){c.dir='<';c.turn=1;}else if(c.turn==1)c.turn=2;else{c.dir='>';c.turn=0;}}c.y--;return c;}
static Cart mvDown (char t[1000][1000], Cart c){ char n=t[c.y+1][c.x];
    if(n=='/')c.dir='<';else if(n=='\\')c.dir='>';else if(n=='+'){if(!c.turn){c.dir='>';c.turn=1;}else if(c.turn==1)c.turn=2;else{c.dir='<';c.turn=0;}}c.y++;return c;}
int main(int argc,char**argv){
    char trk[1000][1000]; Cart cts[1000]; int cnt=0,Y=0;
    NSError*e=nil; NSString*s=[NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&e];
    if(!s){NSLog(@"%@",e);return 1;}
    NSArray*l=[s componentsSeparatedByString:@"\n"];
    for(NSString*ln in l){if([ln length]==0)continue;
        strcpy(trk[Y],[ln UTF8String]);
        for(int X=0;X<[ln length];X++){
            char C=trk[Y][X];if(C=='>'||C=='<'||C=='^'||C=='v'){
                cts[cnt].x=X;cts[cnt].y=Y;cts[cnt].dir=C;cts[cnt].turn=0;cnt++;
            }
        }Y++;
    }
    while(1){
        for(int i=0;i<cnt;i++){
            switch(cts[i].dir){
                case '>':cts[i]=mvRight(trk,cts[i]);break;
                case '<':cts[i]=mvLeft (trk,cts[i]);break;
                case '^':cts[i]=mvUp   (trk,cts[i]);break;
                case 'v':cts[i]=mvDown (trk,cts[i]);break;
            }
        }
        for(int i=0;i<cnt;i++)for(int j=i+1;j<cnt;j++)
            if(cts[i].x==cts[j].x&&cts[i].y==cts[j].y){printf("%d,%d\n",cts[i].x,cts[i].y);return 0;}
    }
}
