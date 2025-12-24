
#import <Foundation/Foundation.h>
#import <stdio.h>
#import <stdlib.h>
#import <string.h>
#import <ctype.h>

#define MAX_LINE_LENGTH 256

typedef struct {
    char **data;
    int rows;
    int cols;
} Ground;

static int strToInt(const char *s){
    int r=0, sign=1, i=0;
    if(s[0]=='-'){sign=-1;i++;}
    for(;s[i];i++) if(isdigit(s[i])) r=r*10+(s[i]-'0');
    return r*sign;
}
static char** allocGround(int r,int c){
    char **g=malloc(r*sizeof(char*));
    if(!g) return NULL;
    for(int i=0;i<r;i++){
        g[i]=malloc(c*sizeof(char));
        if(!g[i]){
            while(--i>=0) free(g[i]);
            free(g);
            return NULL;
        }
    }
    return g;
}
static void freeGround(Ground *g){
    if(g&&g->data){
        for(int i=0;i<g->rows;i++) free(g->data[i]);
        free(g->data);
    }
}
static Ground createGround(int r,int c){
    Ground g={allocGround(r,c),r,c};
    if(!g.data){fprintf(stderr,"alloc fail\n");exit(1);}
    for(int i=0;i<r;i++) for(int j=0;j<c;j++) g.data[i][j]='.';
    g.data[0][0]='+';
    return g;
}
static void growGround(Ground *g,int nr,int nc){
    char **nd=allocGround(nr,nc);
    if(!nd){fprintf(stderr,"alloc fail\n");exit(1);}
    for(int i=0;i<g->rows;i++){
        memcpy(nd[i],g->data[i],g->cols);
        memset(nd[i]+g->cols,'.',nc-g->cols);
    }
    for(int i=g->rows;i<nr;i++) memset(nd[i],'.',nc);
    freeGround(g);
    g->data=nd; g->rows=nr; g->cols=nc;
}
static void shiftGround(Ground *g,int sh){
    int nc=g->cols+sh;
    char **nd=allocGround(g->rows,nc);
    if(!nd){fprintf(stderr,"alloc fail\n");exit(1);}
    for(int i=0;i<g->rows;i++){
        memset(nd[i],'.',sh);
        memcpy(nd[i]+sh,g->data[i],g->cols);
    }
    freeGround(g);
    g->data=nd; g->cols=nc;
}
static void parseLine(Ground *g,char *line,int *minX,int *maxX,int *minY,int *maxY,int xOff,int yOff){
    char *tok; char *arr[6]; int i=0;
    tok=strtok(line,"=, .");
    while(tok&&i<6){arr[i++]=tok;tok=strtok(NULL,"=, .");}
    if(arr[0][0]=='x'){
        int x=strToInt(arr[1])-xOff;
        int y1=strToInt(arr[3])-yOff;
        int y2=strToInt(arr[4])-yOff;
        while(x>=*maxX){*maxX+=1;growGround(g,g->rows,*maxX-*minX+1);}
        while(x<=*minX){*minX-=1;shiftGround(g,1);}
        while(y2>*maxY){*maxY+=1;growGround(g,*maxY+1,g->cols);}
        if(y1<*minY)*minY=y1;
        for(int y=y1;y<=y2;y++) g->data[y][x-*minX]='#';
    }else{
        int y=strToInt(arr[1])-yOff;
        int x1=strToInt(arr[3])-xOff;
        int x2=strToInt(arr[4])-xOff;
        while(y>*maxY){*maxY+=1;growGround(g,*maxY+1,g->cols);}
        while(x2>=*maxX){*maxX+=1;growGround(g,g->rows,*maxX-*minX+1);}
        while(x1<=*minX){*minX-=1;shiftGround(g,1);}
        for(int x=x1;x<=x2;x++) g->data[y][x-*minX]='#';
        if(y<*minY)*minY=y;
    }
}
int main(int argc,const char *argv[]){
    @autoreleasepool{
        NSString *path=@"input.txt";
        NSError *err=nil;
        NSString *content=[NSString stringWithContentsOfFile:path encoding:NSUTF8StringEncoding error:&err];
        if(!content){perror("open");return 1;}
        NSArray<NSString*> *lines=[content componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]];
        Ground g=createGround(1,1);
        int maxX=0,minX=0,maxY=0,minY=20;
        int xOff=500,yOff=0;
        for(NSString *ln in lines){
            if(ln.length==0) continue;
            char buf[MAX_LINE_LENGTH];
            strncpy(buf,ln.UTF8String,MAX_LINE_LENGTH-1);
            buf[MAX_LINE_LENGTH-1]=0;
            parseLine(&g,buf,&minX,&maxX,&minY,&maxY,xOff,yOff);
        }
        int water=0,flow=0,limit=200000;
        int x,y,tryL;
        while(g.data[1][-minX]!='|' && water<limit){
            x=-minX; y=1; tryL=0;
            int can=1;
            while(can){
                if(y+1>maxY||g.data[y+1][x]=='|'){
                    g.data[y][x]='|';can=0;if(y>=minY)flow++;
                }else if(g.data[y+1][x]=='.'){
                    y++;tryL=0;
                }else if(g.data[y+1][x]=='#'||g.data[y+1][x]=='~'){
                    if((tryL==1&&g.data[y][x-1]=='|')||(tryL==2&&g.data[y][x+1]=='|')||
                       (g.data[y][x+1]=='|'&&g.data[y][x-1]!='.')||
                       (g.data[y][x+1]!='.'&&g.data[y][x-1]=='|')){
                        g.data[y][x]='|';flow++;can=0;
                        for(int i=x+1;g.data[y][i]=='~';i++){g.data[y][i]='|';water--;flow++;}
                        for(int i=x-1;g.data[y][i]=='~';i--){g.data[y][i]='|';water--;flow++;}
                    }else if((tryL==0&&g.data[y][x-1]=='.')||(tryL==1&&g.data[y][x-1]=='.')){
                        x--;tryL=1;
                    }else if((tryL==0&&g.data[y][x+1]=='.')||(tryL==2&&g.data[y][x+1]=='.')){
                        x++;tryL=2;
                    }else{
                        can=0;g.data[y][x]='~';water++;
                    }
                }
            }
        }
        printf("%d\n",flow+water);
        freeGround(&g);
    }
    return 0;
}
