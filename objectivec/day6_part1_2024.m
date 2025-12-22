
#import <Foundation/Foundation.h>
#define MAX 1000

int main(int argc, const char * argv[]) {
    FILE *fp = fopen("input.txt", "r");
    if (!fp) return 1;
    static char grid[MAX][MAX];
    int h = 0, w = 0;
    char line[MAX];
    while (fgets(line, sizeof(line), fp)) {
        int len = (int)strlen(line);
        if (len && line[len-1]=='\n') line[--len]=0;
        memcpy(grid[h], line, len+1);
        if (h==0) w=len;
        h++;
    }
    fclose(fp);
    int x=0,y=0,dirIdx=0;
    for (int i=0;i<h;i++) for (int j=0;j<w;j++) {
        switch(grid[i][j]) {
            case '^': x=j; y=i; dirIdx=0; i=h; break;
            case '>': x=j; y=i; dirIdx=1; i=h; break;
            case 'v': x=j; y=i; dirIdx=2; i=h; break;
            case '<': x=j; y=i; dirIdx=3; i=h; break;
        }
    }
    int dirs[4][2] = {{0,-1},{1,0},{0,1},{-1,0}};
    int dirX=dirs[dirIdx][0], dirY=dirs[dirIdx][1];
    static bool visited[MAX][MAX] = {0};
    int count=0;
    visited[y][x]=true; count++;
    while (1) {
        int nx=x+dirX, ny=y+dirY;
        if (nx<0||nx>=w||ny<0||ny>=h) break;
        if (grid[ny][nx]=='#') {
            dirIdx=(dirIdx+1)%4;
            dirX=dirs[dirIdx][0]; dirY=dirs[dirIdx][1];
            continue;
        }
        x=nx; y=ny;
        if (!visited[y][x]) { visited[y][x]=true; count++; }
    }
    printf("%d\n", count);
    return 0;
}
