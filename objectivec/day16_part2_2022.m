
#import <Foundation/Foundation.h>

#define MAX_VALVES 60
#define TIME_LIMIT 26
#define INF 1000000

typedef struct {
    char id[3];
    int flow;
    int index;
    int openMaskIdx;
} Valve;

Valve valves[MAX_VALVES];
int numValves = 0;
int dist[MAX_VALVES][MAX_VALVES];
int openValveIndices[MAX_VALVES];
int numOpenValves = 0;
int memo[MAX_VALVES][TIME_LIMIT + 1][1 << 16];

int findValveIndex(NSString *id) {
    for (int i = 0; i < numValves; i++)
        if ([[NSString stringWithUTF8String:valves[i].id] isEqualToString:id])
            return i;
    return -1;
}

int max(int a, int b) { return a > b ? a : b; }

int solve(int currIdx, int timeLeft, int openMask) {
    if (timeLeft <= 0) return 0;
    if (memo[currIdx][timeLeft][openMask] != -1)
        return memo[currIdx][timeLeft][openMask];
    int maxPressure = 0;
    for (int i = 0; i < numOpenValves; i++) {
        if ((openMask >> i) & 1) {
            int nextIdx = openValveIndices[i];
            int travel = dist[currIdx][nextIdx];
            int t = timeLeft - travel - 1;
            if (t > 0) {
                int pressure = valves[nextIdx].flow * t;
                int remainingMask = openMask & ~(1 << i);
                maxPressure = max(maxPressure, pressure + solve(nextIdx, t, remainingMask));
            }
        }
    }
    return memo[currIdx][timeLeft][openMask] = maxPressure;
}

int main(int argc, const char *argv[]) {
    @autoreleasepool {
        NSString *text = [NSString stringWithContentsOfFile:@"input.txt"
                                                   encoding:NSUTF8StringEncoding
                                                      error:nil];
        for (int i = 0; i < MAX_VALVES; i++)
            for (int j = 0; j < MAX_VALVES; j++)
                dist[i][j] = (i == j) ? 0 : INF;
        for (NSString *line in [text componentsSeparatedByString:@"\n"]) {
            if (!line.length) continue;
            NSString *id = [line substringWithRange:NSMakeRange(6, 2)];
            int flow = [[line substringWithRange:NSMakeRange([line rangeOfString:@";"].location - 2, 2)] intValue];
            int idx = findValveIndex(id);
            if (idx == -1) {
                idx = numValves++;
                strncpy(valves[idx].id, [id UTF8String], 2);
                valves[idx].index = idx;
            }
            valves[idx].flow = flow;
            NSString *tunnels = [line substringFromIndex:[line rangeOfString:@"valve"].location +
                                  ([line containsString:@"valves"] ? 6 : 5)];
            for (NSString *t in [tunnels componentsSeparatedByString:@", "]) {
                t = [t stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceCharacterSet]];
                int nidx = findValveIndex(t);
                if (nidx == -1) {
                    nidx = numValves++;
                    strncpy(valves[nidx].id, [t UTF8String], 2);
                    valves[nidx].index = nidx;
                }
                dist[idx][nidx] = 1;
            }
        }
        for (int k = 0; k < numValves; k++)
            for (int i = 0; i < numValves; i++)
                for (int j = 0; j < numValves; j++)
                    if (dist[i][k] != INF && dist[k][j] != INF)
                        dist[i][j] = MIN(dist[i][j], dist[i][k] + dist[k][j]);
        numOpenValves = 0;
        for (int i = 0; i < numValves; i++)
            if (valves[i].flow > 0) {
                valves[i].openMaskIdx = numOpenValves;
                openValveIndices[numOpenValves++] = i;
            }
        memset(memo, -1, sizeof(memo));
        int start = findValveIndex(@"AA");
        int totalMasks = 1 << numOpenValves;
        int best = 0;
        for (int m = 0; m < totalMasks; m++)
            best = max(best, solve(start, TIME_LIMIT, m) + solve(start, TIME_LIMIT, (totalMasks - 1) ^ m));
        printf("%d\n", best);
    }
    return 0;
}
