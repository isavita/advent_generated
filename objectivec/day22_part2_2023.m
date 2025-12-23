
#import <Foundation/Foundation.h>
#import <stdio.h>
#import <stdlib.h>
#import <string.h>
#import <stdbool.h>

#define MAX_BRICKS 2000

typedef struct {int x,y,z;} Coord;
typedef struct Brick {
    Coord mini;
    Coord maxi;
    struct Brick *basedOn[MAX_BRICKS];
    int basedOnCount;
    struct Brick *support[MAX_BRICKS];
    int supportCount;
} Brick;

static inline int max(int a,int b){return a>b?a:b;}
static inline int min(int a,int b){return a<b?a:b;}

int compareBricks(const void *a,const void *b){
    Brick * const *pa=a,* const *pb=b;
    return (*pa)->maxi.z-(*pb)->maxi.z;
}

Brick *parseInput(NSString *content,int *brickCount){
    Brick *bricks=malloc(sizeof(Brick)*MAX_BRICKS);
    NSArray *lines=[content componentsSeparatedByString:@"\n"];
    int i=0;
    for(NSString *line in lines){
        if(line.length==0)continue;
        sscanf(line.UTF8String,"%d,%d,%d~%d,%d,%d",
               &bricks[i].mini.x,&bricks[i].mini.y,&bricks[i].mini.z,
               &bricks[i].maxi.x,&bricks[i].maxi.y,&bricks[i].maxi.z);
        bricks[i].basedOnCount=bricks[i].supportCount=0;
        i++;
    }
    *brickCount=i;
    return bricks;
}

void settle(Brick *bricks,int brickCount){
    Brick *ptrs[MAX_BRICKS];
    for(int i=0;i<brickCount;i++)ptrs[i]=&bricks[i];
    qsort(ptrs,brickCount,sizeof(Brick*),compareBricks);
    for(int i=0;i<brickCount;i++){
        Brick *b=ptrs[i];
        int supportZ=0,baseCnt=0;
        for(int j=i-1;j>=0;j--){
            Brick *p=ptrs[j];
            bool ix=max(b->mini.x,p->mini.x)<=min(b->maxi.x,p->maxi.x);
            bool iy=max(b->mini.y,p->mini.y)<=min(b->maxi.y,p->maxi.y);
            if(ix&&iy){
                if(p->maxi.z==supportZ){
                    b->basedOn[baseCnt++]=p;
                }else if(p->maxi.z>supportZ){
                    supportZ=p->maxi.z;
                    baseCnt=0;
                    b->basedOn[baseCnt++]=p;
                }
            }
        }
        b->basedOnCount=baseCnt;
        for(int j=0;j<baseCnt;j++){
            Brick *p=b->basedOn[j];
            p->support[p->supportCount++]=b;
        }
        int dz=b->maxi.z-b->mini.z;
        b->mini.z=supportZ+1;
        b->maxi.z=b->mini.z+dz;
    }
}

int solve(Brick *bricks,int brickCount){
    settle(bricks,brickCount);
    int total=0;
    for(int i=0;i<brickCount;i++){
        Brick *b=&bricks[i];
        int falling[MAX_BRICKS]={0},fallCnt=0;
        for(int j=0;j<b->supportCount;j++){
            Brick *s=b->support[j];
            if(s->basedOnCount!=1)continue;
            Brick *stack[MAX_BRICKS];int sp=0;
            stack[sp++]=s;
            while(sp){
                Brick *cur=stack[--sp];
                bool drop=true;
                for(int k=0;k<cur->basedOnCount;k++){
                    Brick *base=cur->basedOn[k];
                    bool inFall=false;
                    for(int m=0;m<fallCnt;m++)if(&bricks[falling[m]]==base){inFall=true;break;}
                    if(base!=b && !inFall){drop=false;break;}
                }
                if(drop){
                    bool already=false;
                    for(int m=0;m<fallCnt;m++)if(&bricks[falling[m]]==cur){already=true;break;}
                    if(!already){
                        falling[fallCnt++]=cur-bricks;
                        for(int k=0;k<cur->supportCount;k++)stack[sp++]=cur->support[k];
                    }
                }
            }
        }
        total+=fallCnt;
    }
    return total;
}

int main(int argc,char *argv[]){
    @autoreleasepool{
        NSString *path=@"input.txt";
        NSError *err=nil;
        NSString *content=[NSString stringWithContentsOfFile:path encoding:NSUTF8StringEncoding error:&err];
        if(!content){fprintf(stderr,"Failed to read file\n");return 1;}
        int count;
        Brick *bricks=parseInput(content,&count);
        int result=solve(bricks,count);
        free(bricks);
        printf("%d\n",result);
    }
    return 0;
}
