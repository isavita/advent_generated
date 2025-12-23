
#import <Foundation/Foundation.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>

#define MAX_GRID_SIZE 200
#define MAX_LINE_LENGTH 200

typedef struct {int x;int y;} Coord;
typedef unsigned char Tile;
typedef struct {bool top;bool right;bool bottom;bool left;} Pipe;

typedef struct {
    int width;
    int height;
    Tile data[MAX_GRID_SIZE][MAX_GRID_SIZE];
} Grid;

const Tile Empty='.';
const Tile Start='S';
const Tile Vertical='|';
const Tile Horizontal='-';
const Tile TopLeftCorner='J';
const Tile TopRightCorner='L';
const Tile BottomLeftCorner='7';
const Tile BottomRightCorner='F';
const Tile Enclosed='X';

Pipe VerticalPipe={true,false,true,false};
Pipe HorizontalPipe={false,true,false,true};
Pipe TopLeftCornerPipe={true,false,false,true};
Pipe TopRightCornerPipe={true,true,false,false};
Pipe BottomLeftCornerPipe={false,false,true,true};
Pipe BottomRightCornerPipe={false,true,true,false};

static const Coord Top={0,-1},Right={1,0},Bottom={0,1},Left={-1,0},Undefined={0,0};

Pipe getPipeFromTile(Tile t){
    switch(t){
        case Vertical:return VerticalPipe;
        case Horizontal:return HorizontalPipe;
        case TopLeftCorner:return TopLeftCornerPipe;
        case TopRightCorner:return TopRightCornerPipe;
        case BottomLeftCorner:return BottomLeftCornerPipe;
        case BottomRightCorner:return BottomRightCornerPipe;
        default:{Pipe p={false,false,false,false};return p;}
    }
}
Tile getTileFromPipe(Pipe p){
    if(memcmp(&p,&VerticalPipe,sizeof(Pipe))==0)return Vertical;
    if(memcmp(&p,&HorizontalPipe,sizeof(Pipe))==0)return Horizontal;
    if(memcmp(&p,&TopLeftCornerPipe,sizeof(Pipe))==0)return TopLeftCorner;
    if(memcmp(&p,&TopRightCornerPipe,sizeof(Pipe))==0)return TopRightCorner;
    if(memcmp(&p,&BottomLeftCornerPipe,sizeof(Pipe))==0)return BottomLeftCorner;
    if(memcmp(&p,&BottomRightCornerPipe,sizeof(Pipe))==0)return BottomRightCorner;
    return Empty;
}
Grid buildGrid(char *in[],int lines){
    Grid g;g.height=lines;g.width=strlen(in[0]);
    for(int y=0;y<lines;y++)for(int x=0;x<g.width;x++){
        g.data[y][x]=(in[y][x]==Empty?Empty:in[y][x]);
    }
    return g;
}
Coord findStart(Grid g){
    for(int y=0;y<g.height;y++)for(int x=0;x<g.width;x++)if(g.data[y][x]==Start)return (Coord){x,y};
    return Undefined;
}
Pipe getPipeFromNeighbors(Coord c,Grid g){
    Pipe p={false,false,false,false};
    Coord nb[4]={{c.x,c.y-1},{c.x+1,c.y},{c.x,c.y+1},{c.x-1,c.y}};
    Coord dir[4]={Top,Right,Bottom,Left};
    for(int i=0;i<4;i++){
        Coord n=nb[i];
        if(n.x>=0&&n.x<g.width&&n.y>=0&&n.y<g.height){
            Pipe np=getPipeFromTile(g.data[n.y][n.x]);
            if((dir[i].x==Top.x&&dir[i].y==Top.y&&np.bottom)||
               (dir[i].x==Right.x&&dir[i].y==Right.y&&np.left)||
               (dir[i].x==Bottom.x&&dir[i].y==Bottom.y&&np.top)||
               (dir[i].x==Left.x&&dir[i].y==Left.y&&np.right)){
                if(dir[i].x==Top.x&&dir[i].y==Top.y)p.top=true;
                if(dir[i].x==Right.x&&dir[i].y==Right.y)p.right=true;
                if(dir[i].x==Bottom.x&&dir[i].y==Bottom.y)p.bottom=true;
                if(dir[i].x==Left.x&&dir[i].y==Left.y)p.left=true;
            }
        }
    }
    return p;
}
int pathFinding(Coord s,Grid g,Coord *path){
    path[0]=s;
    Pipe sp=getPipeFromNeighbors(s,g);
    Coord prev,cur;
    int len=1;
    if(sp.top){prev=Top;cur=(Coord){s.x+Top.x,s.y+Top.y};}
    else if(sp.right){prev=Right;cur=(Coord){s.x+Right.x,s.y+Right.y};}
    else if(sp.bottom){prev=Bottom;cur=(Coord){s.x+Bottom.x,s.y+Bottom.y};}
    else {prev=Left;cur=(Coord){s.x+Left.x,s.y+Left.y};}
    while(cur.x!=s.x||cur.y!=s.y){
        path[len++]=cur;
        Pipe cp=getPipeFromTile(g.data[cur.y][cur.x]);
        if(cp.top && !(prev.x==Bottom.x&&prev.y==Bottom.y)){prev=Top;cur.x+=Top.x;cur.y+=Top.y;}
        else if(cp.right && !(prev.x==Left.x&&prev.y==Left.y)){prev=Right;cur.x+=Right.x;cur.y+=Right.y;}
        else if(cp.bottom && !(prev.x==Top.x&&prev.y==Top.y)){prev=Bottom;cur.x+=Bottom.x;cur.y+=Bottom.y;}
        else if(cp.left && !(prev.x==Right.x&&prev.y==Right.y)){prev=Left;cur.x+=Left.x;cur.y+=Left.y;}
    }
    return len;
}
int solve(char *in[],int lines){
    Grid g=buildGrid(in,lines);
    Coord start=findStart(g);
    Coord path[MAX_GRID_SIZE*MAX_GRID_SIZE];
    int len=pathFinding(start,g,path);
    return len/2;
}
int main(int argc,char *argv[]){
    @autoreleasepool{
        NSString *content=[NSString stringWithContentsOfFile:@"input.txt"
                                                    encoding:NSUTF8StringEncoding
                                                       error:nil];
        NSArray<NSString*> *rows=[content componentsSeparatedByCharactersInSet:
                                   [NSCharacterSet newlineCharacterSet]];
        char *input[MAX_GRID_SIZE];
        int lines=0;
        for(NSString *line in rows){
            if(line.length==0)continue;
            char *cstr=strdup(line.UTF8String);
            input[lines++]=cstr;
            if(lines>=MAX_GRID_SIZE)break;
        }
        int result=solve(input,lines);
        printf("%d\n",result);
        for(int i=0;i<lines;i++)free(input[i]);
    }
    return 0;
}
