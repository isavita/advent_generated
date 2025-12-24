
#import <Foundation/Foundation.h>
#import <stdio.h>
#import <stdlib.h>
#import <string.h>
#import <stdbool.h>
#import <limits.h>

typedef struct {
    bool isOn;
    int x1, x2;
    int y1, y2;
    int z1, z2;
} Cube;

static inline int max(int a,int b){return a>b?a:b;}
static inline int min(int a,int b){return a<b?a:b;}

static inline long long cubeVolume(Cube c){
    long long v=(long long)(c.x2-c.x1+1)*(c.y2-c.y1+1)*(c.z2-c.z1+1);
    return c.isOn?v:-v;
}

static bool getIntersection(Cube c1,Cube c2,Cube *i){
    int x1=max(c1.x1,c2.x1),x2=min(c1.x2,c2.x2);
    int y1=max(c1.y1,c2.y1),y2=min(c1.y2,c2.y2);
    int z1=max(c1.z1,c2.z1),z2=min(c1.z2,c2.z2);
    if(x1>x2||y1>y2||z1>z2)return false;
    bool s;
    if(c1.isOn&&c2.isOn)s=false;
    else if(!c1.isOn&&!c2.isOn)s=true;
    else s=c2.isOn;
    *i=(Cube){s,x1,x2,y1,y2,z1,z2};
    return true;
}

static Cube* parseInput(char *data,int *cnt){
    *cnt=0;
    int cap=16;
    Cube *arr=malloc(cap*sizeof(Cube));
    char *line=strtok(data,"\n");
    while(line){
        char onOff[4];
        int x1,x2,y1,y2,z1,z2;
        if(sscanf(line,"%3s x=%d..%d,y=%d..%d,z=%d..%d",onOff,&x1,&x2,&y1,&y2,&z1,&z2)!=7) exit(1);
        if(*cnt==cap){cap*=2;arr=realloc(arr,cap*sizeof(Cube));}
        arr[(*cnt)++] = (Cube){strcmp(onOff,"on")==0,x1,x2,y1,y2,z1,z2};
        line=strtok(NULL,"\n");
    }
    return arr;
}

static long long solve(Cube *cubes,int n){
    int fCap=16,fCnt=0;
    Cube *final=malloc(fCap*sizeof(Cube));
    for(int i=0;i<n;i++){
        Cube cur=cubes[i];
        int aCap=16,aCnt=0;
        Cube *add=malloc(aCap*sizeof(Cube));
        for(int j=0;j<fCnt;j++){
            Cube inter;
            if(getIntersection(final[j],cur,&inter)){
                if(aCnt==aCap){aCap*=2;add=realloc(add,aCap*sizeof(Cube));}
                add[aCnt++]=inter;
            }
        }
        if(cur.isOn){
            if(aCnt==aCap){aCap*=2;add=realloc(add,aCap*sizeof(Cube));}
            add[aCnt++]=cur;
        }
        for(int k=0;k<aCnt;k++){
            if(fCnt==fCap){fCap*=2;final=realloc(final,fCap*sizeof(Cube));}
            final[fCnt++]=add[k];
        }
        free(add);
    }
    long long total=0;
    for(int i=0;i<fCnt;i++) total+=cubeVolume(final[i]);
    free(final);
    return total;
}

int main(int argc,char *argv[]){
    @autoreleasepool{
        FILE *fp=fopen("input.txt","r");
        if(!fp)return 1;
        fseek(fp,0,SEEK_END);
        long sz=ftell(fp);
        fseek(fp,0,SEEK_SET);
        char *buf=malloc(sz+1);
        fread(buf,1,sz,fp);
        fclose(fp);
        buf[sz]=0;
        int cnt;
        Cube *cubes=parseInput(buf,&cnt);
        long long ans=solve(cubes,cnt);
        printf("%lld\n",ans);
        free(cubes);
        free(buf);
    }
    return 0;
}
