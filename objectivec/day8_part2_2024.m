
#import <Foundation/Foundation.h>
#import <stdio.h>
#import <stdlib.h>
#import <string.h>
#import <stdbool.h>

#define MAX_H 500
#define MAX_W 500

typedef struct {int y;int x;} Coord;

static char grid[MAX_H][MAX_W+2];
static Coord antenna_coords[256][MAX_H*MAX_W];
static int antenna_counts[256];
static bool is_antinode[MAX_H][MAX_W];

static long long gcd_ll(long long a,long long b){
    a = llabs(a); b = llabs(b);
    while(b){ a %= b; long long t=a;a=b;b=t; }
    return a;
}

int main(int argc, const char * argv[]) {
    FILE *fp = fopen("input.txt","r");
    if(!fp){ printf("0\n"); return 0; }

    int h=0,w=0;
    while(h<MAX_H && fgets(grid[h],MAX_W+2,fp)){
        grid[h][strcspn(grid[h],"\r\n")]=0;
        if(strlen(grid[h])==0) continue;
        if(h==0){ w=strlen(grid[0]); if(w>MAX_W){w=MAX_W;grid[0][w]=0;} }
        h++;
    }
    fclose(fp);
    if(h==0||w==0){ printf("0\n"); return 0; }

    for(int y=0;y<h;y++)for(int x=0;x<w;x++)if(grid[y][x]!='.'){
        unsigned char idx=grid[y][x];
        int c=antenna_counts[idx];
        antenna_coords[idx][c]=(Coord){y,x};
        antenna_counts[idx]=c+1;
    }

    long long antinode_count=0;
    for(int f=0;f<256;f++){
        int n=antenna_counts[f];
        if(n<2) continue;
        Coord *cfs=antenna_coords[f];
        for(int i=0;i<n;i++)for(int j=i+1;j<n;j++){
            Coord A=cfs[i],B=cfs[j];
            long long dy=(long long)B.y-A.y,dx=(long long)B.x-A.x;
            long long d=gcd_ll(dy,dx);
            long long sy=dy/d,sx=dx/d;
            if(sx<0||(sx==0&&sy<0)){sx=-sx;sy=-sy;}
            long long c=sy*A.x-sx*A.y;

            if(sy==0){
                if(sx==0) continue;
                if(llabs(c)%sx==0){
                    long long yline=-c/sx;
                    if(yline>=0&&yline<h){
                        int iy=yline;
                        for(int ix=0;ix<w;ix++)if(!is_antinode[iy][ix]){
                            is_antinode[iy][ix]=true; antinode_count++;
                        }
                    }
                }
            }else if(sx==0){
                if(llabs(c)%sy==0){
                    long long xline=c/sy;
                    if(xline>=0&&xline<w){
                        int ix=xline;
                        for(int iy=0;iy<h;iy++)if(!is_antinode[iy][ix]){
                            is_antinode[iy][ix]=true; antinode_count++;
                        }
                    }
                }
            }else{
                for(int iy=0;iy<h;iy++){
                    long long num=c+sx*iy;
                    if(num%sy) continue;
                    long long xline=num/sy;
                    if(xline>=0&&xline<w){
                        int ix=xline;
                        if(!is_antinode[iy][ix]){
                            is_antinode[iy][ix]=true; antinode_count++;
                        }
                    }
                }
            }
        }
    }
    printf("%lld\n",antinode_count);
    return 0;
}
