#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

typedef struct { int r,c; } Point;
typedef struct { Point *p; int n; } Piece;

char *trim(char *s){
    while(*s && isspace((unsigned char)*s)) s++;
    char *e = s + strlen(s) - 1;
    while(e>=s && isspace((unsigned char)*e)) *e-- = 0;
    return s;
}

int cmpPoint(const void *a,const void *b){
    Point A=*(Point*)a, B=*(Point*)b;
    if(A.r==B.r) return A.c-B.c;
    return A.r-B.r;
}

Piece normalize(Piece in){
    Piece out; out.n=in.n; out.p=malloc(sizeof(Point)*out.n);
    if(out.n==0) return out;
    int minr=in.p[0].r, minc=in.p[0].c;
    for(int i=0;i<in.n;i++){ if(in.p[i].r<minr) minr=in.p[i].r; if(in.p[i].c<minc) minc=in.p[i].c; }
    for(int i=0;i<in.n;i++){ out.p[i].r=in.p[i].r-minr; out.p[i].c=in.p[i].c-minc; }
    qsort(out.p,out.n,sizeof(Point),cmpPoint);
    return out;
}

Piece rotateP(Piece a){
    Piece b; b.n=a.n; b.p=malloc(sizeof(Point)*b.n);
    for(int i=0;i<b.n;i++){ b.p[i].r=a.p[i].c; b.p[i].c=-a.p[i].r; }
    return b;
}
Piece flipP(Piece a){
    Piece b; b.n=a.n; b.p=malloc(sizeof(Point)*b.n);
    for(int i=0;i<b.n;i++){ b.p[i].r=a.p[i].r; b.p[i].c=-a.p[i].c; }
    return b;
}

int pieceEqual(Piece *a, Piece *b){
    if(a->n!=b->n) return 0;
    for(int i=0;i<a->n;i++) if(a->p[i].r!=b->p[i].r || a->p[i].c!=b->p[i].c) return 0;
    return 1;
}

Piece *generateVariations(Piece base, int *outCount){
    Piece uniq[32]; int uc=0;
    Piece curr = base;
    for(int i=0;i<4;i++){
        Piece n = normalize(curr);
        int ok=1; for(int j=0;j<uc && ok;j++) if(pieceEqual(&uniq[j],&n)) ok=0;
        if(ok) uniq[uc++]=n; else { free(n.p); }
        Piece f = flipP(curr);
        Piece nf = normalize(f); free(f.p);
        ok=1; for(int j=0;j<uc && ok;j++) if(pieceEqual(&uniq[j],&nf)) ok=0;
        if(ok) uniq[uc++]=nf; else { free(nf.p); }
        Piece r = rotateP(curr); if(i>0) { free(curr.p); } curr = r;
    }
    if(curr.p) free(curr.p);
    Piece *res = malloc(sizeof(Piece)*uc);
    for(int i=0;i<uc;i++) res[i]=uniq[i];
    *outCount=uc;
    return res;
}

int canPlace(int rows,int cols,char *grid,Piece p,int rr,int cc){
    for(int i=0;i<p.n;i++){
        int nr=rr+p.p[i].r, nc=cc+p.p[i].c;
        if(nr<0||nr>=rows||nc<0||nc>=cols) return 0;
        if(grid[nr*cols+nc]) return 0;
    }
    return 1;
}
void place(int cols,char *grid,Piece p,int rr,int cc,char val){
    for(int i=0;i<p.n;i++) grid[(rr+p.p[i].r)*cols + (cc+p.p[i].c)] = val;
}

int checkIslands(int rows,int cols,char *grid,int *counts,int arrSize,int slackIdx, Piece *shapes){
    int minReal=1<<30; int hasReal=0;
    for(int i=0;i<arrSize;i++) if(i!=slackIdx && counts[i]>0){ if(shapes[i].n < minReal) minReal=shapes[i].n; hasReal=1; }
    if(!hasReal) return 1;
    int availSlack = counts[slackIdx];
    char *vis = malloc(rows*cols); memset(vis,0,rows*cols);
    int *q = malloc(sizeof(int)*rows*cols);
    for(int i=0;i<rows*cols;i++){
        if(!grid[i] && !vis[i]){
            int qs=0,qe=0;
            q[qe++]=i; vis[i]=1;
            int size=0;
            while(qs<qe){
                int cur=q[qs++]; size++;
                int r=cur/cols,c=cur%cols;
                if(r>0){ int n=(r-1)*cols+c; if(!grid[n] && !vis[n]){ vis[n]=1; q[qe++]=n; } }
                if(r<rows-1){ int n=(r+1)*cols+c; if(!grid[n] && !vis[n]){ vis[n]=1; q[qe++]=n; } }
                if(c>0){ int n=r*cols+(c-1); if(!grid[n] && !vis[n]){ vis[n]=1; q[qe++]=n; } }
                if(c<cols-1){ int n=r*cols+(c+1); if(!grid[n] && !vis[n]){ vis[n]=1; q[qe++]=n; } }
            }
            if(size < minReal){
                if(availSlack >= size) availSlack -= size;
                else { free(vis); free(q); return 0; }
            }
        }
    }
    free(vis); free(q);
    return 1;
}

int solveRec(int rows,int cols,char *grid,int *counts,int arrSize,int *ids,int idCount, Piece **variations,int *varCounts, int slackIdx, Piece *shapes){
    int empty=-1;
    for(int i=0;i<rows*cols;i++) if(!grid[i]) { empty=i; break; }
    if(empty==-1) return 1;
    int r=empty/cols,c=empty%cols;
    if(!checkIslands(rows,cols,grid,counts,arrSize,slackIdx,shapes)) return 0;
    for(int ii=0;ii<idCount;ii++){
        int id=ids[ii];
        if(counts[id]==0) continue;
        counts[id]--;
        for(int v=0;v<varCounts[id];v++){
            Piece p = variations[id][v];
            if(canPlace(rows,cols,grid,p,r,c)){
                place(cols,grid,p,r,c,1);
                if(solveRec(rows,cols,grid,counts,arrSize,ids,idCount,variations,varCounts,slackIdx,shapes)) return 1;
                place(cols,grid,p,r,c,0);
            }
        }
        counts[id]++;
    }
    return 0;
}

int main(){
    FILE *f = fopen("input.txt","r");
    if(!f) return 1;
    char *line=NULL; size_t sz=0;
    char **lines=NULL; int ln=0;
    while(getline(&line,&sz,f)!=-1){
        lines = realloc(lines,sizeof(char*)*(ln+1));
        lines[ln++] = strdup(line);
    }
    fclose(f); free(line);

    int maxId=-1000000;
    for(int i=0;i<ln;i++){
        char *s = trim(lines[i]);
        int L=strlen(s);
        if(L>0 && s[L-1]==':'){
            char tmp[64]; strcpy(tmp,s); tmp[L-1]=0;
            int id = atoi(tmp);
            if(id>maxId) maxId=id;
        }
    }
    if(maxId<0) maxId = -1;
    int arrSize = maxId+2;
    int slackIdx = maxId+1;

    Piece *shapes = calloc(arrSize,sizeof(Piece));
    int parsingShapes = 1;
    int currentID = -1;
    char **currentShapeLines=NULL; int csl=0;
    char **regionLines=NULL; int rln=0;

    for(int i=0;i<ln;i++){
        char *raw = lines[i];
        char *s = trim(raw);
        if(strlen(s)==0) continue;
        if(strchr(s,'x') && strchr(s,':')) parsingShapes=0;
        if(parsingShapes){
            int L=strlen(s);
            if(L>0 && s[L-1]==':'){
                if(currentID!=-1 && csl>0){
                    int cnt=0;
                    for(int k=0;k<csl;k++) for(int j=0;j<strlen(currentShapeLines[k]);j++) if(currentShapeLines[k][j]=='#') cnt++;
                    Piece p; p.n=0; p.p=NULL;
                    for(int r=0;r<csl;r++){
                        for(int c=0;c<strlen(currentShapeLines[r]);c++) if(currentShapeLines[r][c]=='#'){
                            p.p = realloc(p.p,sizeof(Point)*(p.n+1));
                            p.p[p.n].r = r;
                            p.p[p.n].c = c;
                            p.n++;
                        }
                    }
                    shapes[currentID] = normalize(p);
                    for(int k=0;k<csl;k++) free(currentShapeLines[k]);
                    free(currentShapeLines); currentShapeLines=NULL; csl=0;
                }
                char tmp[64]; strcpy(tmp,s); tmp[L-1]=0;
                currentID = atoi(tmp);
            } else {
                currentShapeLines = realloc(currentShapeLines,sizeof(char*)*(csl+1));
                currentShapeLines[csl++] = strdup(s);
            }
        } else {
            regionLines = realloc(regionLines,sizeof(char*)*(rln+1));
            regionLines[rln++] = strdup(s);
        }
    }
    if(currentID!=-1 && csl>0){
        Piece p; p.n=0; p.p=NULL;
        for(int r=0;r<csl;r++) for(int c=0;c<strlen(currentShapeLines[r]);c++) if(currentShapeLines[r][c]=='#'){
            p.p = realloc(p.p,sizeof(Point)*(p.n+1));
            p.p[p.n].r = r;
            p.p[p.n].c = c;
            p.n++;
        }
        shapes[currentID] = normalize(p);
        for(int k=0;k<csl;k++) free(currentShapeLines[k]);
        free(currentShapeLines);
    }

    for(int i=0;i<ln;i++) free(lines[i]);
    free(lines);

    for(int i=0;i<arrSize;i++) { if(shapes[i].n==0){ shapes[i].p=NULL; shapes[i].n=0; } }
    shapes[slackIdx].n=1; shapes[slackIdx].p=malloc(sizeof(Point)); shapes[slackIdx].p[0].r=0; shapes[slackIdx].p[0].c=0;

    Piece **variations = malloc(sizeof(Piece*)*arrSize);
    int *varCounts = calloc(arrSize,sizeof(int));
    for(int i=0;i<arrSize;i++){
        if(shapes[i].n==0){ variations[i]=NULL; varCounts[i]=0; continue; }
        variations[i] = generateVariations(shapes[i], &varCounts[i]);
    }

    int solvedCount=0;
    for(int i=0;i<rln;i++){
        char *lnstr = regionLines[i];
        char *colon = strchr(lnstr,':');
        if(!colon) continue;
        *colon=0;
        char *dims = lnstr;
        char *countsStr = colon+1;
        trim(dims); trim(countsStr);
        int wx=0,h=0;
        if(sscanf(dims,"%dx%d",&wx,&h)!=2) continue;
        int gridSize = wx*h;
        int *pieceCounts = calloc(arrSize,sizeof(int));
        int totalArea=0;
        char *tok = strtok(countsStr," \t");
        int idx=0;
        while(tok){
            int c = atoi(tok);
            if(c>0){
                if(idx < arrSize-1){
                    pieceCounts[idx]=c;
                    totalArea += c * shapes[idx].n;
                }
            }
            idx++;
            tok = strtok(NULL," \t");
        }
        if(totalArea > gridSize){ free(pieceCounts); continue; }
        int slack = gridSize - totalArea;
        if(slack>0) pieceCounts[slackIdx]=slack;

        int *ids = malloc(sizeof(int)*arrSize); int idc=0;
        for(int j=0;j<arrSize;j++) if(pieceCounts[j]>0) ids[idc++]=j;
        for(int a=0;a<idc;a++) for(int b=a+1;b<idc;b++){
            if(shapes[ids[b]].n > shapes[ids[a]].n){
                int t=ids[a]; ids[a]=ids[b]; ids[b]=t;
            }
        }
        char *grid = calloc(gridSize,1);
        if(solveRec(h,wx,grid,pieceCounts,arrSize,ids,idc,variations,varCounts,slackIdx,shapes)) solvedCount++;
        free(grid); free(ids); free(pieceCounts);
    }

    for(int i=0;i<rln;i++) free(regionLines[i]);
    free(regionLines);

    printf("Number of regions that fit all presents: %d\n", solvedCount);
    return 0;
}