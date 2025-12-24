
#import <Foundation/Foundation.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define ITERATIONS 50
#define EXPAND_BY 1

static char *readAlgorithm(FILE *f){
    char *buf = malloc(513);
    fgets(buf,513,f);
    buf[strcspn(buf,"\n")]=0;
    return buf;
}
static int **readImage(FILE *f,int *h,int *w){
    fscanf(f,"\n");
    *h=*w=0;
    int **img=NULL;
    char line[513];
    while(fgets(line,513,f)){
        int len=strlen(line);
        if(line[len-1]=='\n') line[--len]=0;
        if(!*w) *w=len;
        img=realloc(img,(*h+1)*sizeof(int*));
        img[*h]=malloc(*w*sizeof(int));
        for(int i=0;i<*w;i++) img[*h][i]=line[i]=='#';
        (*h)++;
    }
    return img;
}
static int **enhance(char *alg,int **img,int h,int w,int inf){
    int nh=h+2*EXPAND_BY, nw=w+2*EXPAND_BY;
    int **out=malloc(nh*sizeof(int*));
    for(int i=0;i<nh;i++) out[i]=malloc(nw*sizeof(int));
    for(int y=-EXPAND_BY;y<h+EXPAND_BY;y++)
        for(int x=-EXPAND_BY;x<w+EXPAND_BY;x++){
            int idx=0;
            for(int dy=-1;dy<=1;dy++)
                for(int dx=-1;dx<=1;dx++){
                    idx<<=1;
                    int ny=y+dy,nx=x+dx;
                    if(ny>=0&&ny<h&&nx>=0&&nx<w){
                        if(img[ny][nx]) idx|=1;
                    }else if(inf) idx|=1;
                }
            out[y+EXPAND_BY][x+EXPAND_BY]=alg[idx]=='#';
        }
    return out;
}
static int countLit(int **img,int h,int w){
    int c=0;
    for(int i=0;i<h;i++)
        for(int j=0;j<w;j++) c+=img[i][j];
    return c;
}
int main(int argc,const char * argv[]){
    @autoreleasepool{
        FILE *f=fopen("input.txt","r");
        char *alg=readAlgorithm(f);
        int h,w;
        int **img=readImage(f,&h,&w);
        for(int i=0;i<ITERATIONS;i++){
            img=enhance(alg,img,h,w,i%2 && alg[0]=='#');
            h+=2*EXPAND_BY;
            w+=2*EXPAND_BY;
        }
        printf("%d\n",countLit(img,h,w));
    }
    return 0;
}
