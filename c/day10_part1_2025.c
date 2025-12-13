
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

#define MAXR 64
#define MAXC 64
#define MAXLINE 1024
#define MAXMACH 128

int R, C;
int matrix[MAXR][MAXC+1];
int colIsPivot[MAXC];
int freeVars[MAXC], nFree;
int target[MAXR], buttons[MAXC][MAXR], btnLen[MAXC];
int minWeight;

static inline int bits(int x){
    int c=0;
    while(x){ x&=(x-1); c++; }
    return c;
}

int gaussianEliminationMinWeight(){
    int pivotRow=0;
    memset(colIsPivot,0,sizeof(colIsPivot));
    for(int c=0;c<C && pivotRow<R;c++){
        int sel=-1;
        for(int r=pivotRow;r<R;r++)
            if(matrix[r][c]==1){ sel=r; break; }
        if(sel==-1) continue;
        for(int k=0;k<=C;k++){
            int t=matrix[pivotRow][k];
            matrix[pivotRow][k]=matrix[sel][k];
            matrix[sel][k]=t;
        }
        for(int r=0;r<R;r++){
            if(r!=pivotRow && matrix[r][c]==1){
                for(int k=c;k<=C;k++) matrix[r][k]^=matrix[pivotRow][k];
            }
        }
        colIsPivot[c]=1;
        pivotRow++;
    }
    for(int r=pivotRow;r<R;r++) if(matrix[r][C]==1) return -1;
    nFree=0;
    for(int c=0;c<C;c++) if(!colIsPivot[c]) freeVars[nFree++]=c;
    minWeight=INT_MAX;
    int limit=1<<nFree;
    for(int i=0;i<limit;i++){
        int x[MAXC]={0}, cw=bits(i);
        for(int j=0;j<nFree;j++) if((i>>j)&1) x[freeVars[j]]=1;
        int currPivotRow=0;
        for(int c=0;c<C;c++){
            if(colIsPivot[c]){
                int val=matrix[currPivotRow][C];
                for(int k=c+1;k<C;k++) if(matrix[currPivotRow][k]==1) val^=x[k];
                x[c]=val;
                if(val) cw++;
                currPivotRow++;
            }
        }
        if(cw<minWeight) minWeight=cw;
    }
    return minWeight;
}

int main(){
    FILE* f=fopen("input.txt","r");
    if(!f){ perror("input.txt"); return 1; }
    char line[MAXLINE];
    int totalPresses=0;
    while(fgets(line,sizeof(line),f)){
        char* p=strchr(line,'\n'); if(p) *p=0;
        char* brack=strchr(line,'[');
        if(!brack) continue;
        brack++;
        char* end=strchr(brack,']');
        if(!end) continue;
        *end=0;
        R=strlen(brack);
        for(int i=0;i<R;i++) target[i]=(brack[i]=='#')?1:0;
        C=0;
        char* btn=strstr(end+1,"(");
        while(btn){
            btn++;
            char* e=strchr(btn,')');
            if(!e) break;
            *e=0;
            btnLen[C]=0;
            char* tok=strtok(btn,",");
            while(tok){
                int idx=atoi(tok);
                buttons[C][btnLen[C]++]=idx;
                tok=strtok(NULL,",");
            }
            C++;
            btn=strstr(e+1,"(");
        }
        for(int r=0;r<R;r++){
            for(int c=0;c<C;c++){
                matrix[r][c]=0;
                for(int k=0;k<btnLen[c];k++)
                    if(buttons[c][k]==r){ matrix[r][c]=1; break; }
            }
            matrix[r][C]=target[r];
        }
        int mw=gaussianEliminationMinWeight();
        if(mw!=-1) totalPresses+=mw;
    }
    fclose(f);
    printf("%d\n",totalPresses);
    return 0;
}
