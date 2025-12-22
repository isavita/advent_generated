
#import <Foundation/Foundation.h>

#define favoriteNumber 1362

static inline BOOL isWall(int x, int y) {
    if (x < 0 || y < 0) return YES;
    int num = x*x + 3*x + 2*x*y + y + y*y + favoriteNumber;
    int bits = 0;
    while (num) {
        bits += num & 1;
        num >>= 1;
    }
    return bits & 1;
}

int main(int argc, char *argv[]) {
    @autoreleasepool {
        BOOL visited[100][100] = {0};
        int queueX[10000], queueY[10000];
        int front = 0, rear = 0;
        queueX[rear] = 1; queueY[rear] = 1; rear++;
        visited[1][1] = YES;
        int steps = 0;
        while (front < rear && steps < 50) {
            int size = rear - front;
            for (int i = 0; i < size; i++) {
                int x = queueX[front], y = queueY[front]; front++;
                int dx[] = {1,-1,0,0}, dy[] = {0,0,1,-1};
                for (int d = 0; d < 4; d++) {
                    int nx = x + dx[d], ny = y + dy[d];
                    if (nx >= 0 && ny >= 0 && !isWall(nx, ny) && !visited[nx][ny]) {
                        visited[nx][ny] = YES;
                        queueX[rear] = nx; queueY[rear] = ny; rear++;
                    }
                }
            }
            steps++;
        }
        int count = 0;
        for (int i = 0; i < 100; i++)
            for (int j = 0; j < 100; j++)
                if (visited[i][j]) count++;
        printf("%d\n", count);
    }
    return 0;
}
