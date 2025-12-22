
#import <Foundation/Foundation.h>
#import <math.h>

#define MAX_LINE 1024
#define MAX_COUNTERS 20
#define MAX_BUTTONS 50
#define INF 0x3f3f3f3f

int buttons[MAX_BUTTONS][MAX_COUNTERS];
int btnSize[MAX_BUTTONS];
int targets[MAX_COUNTERS];
int numCounters, numButtons;

double matrix[MAX_COUNTERS][MAX_BUTTONS + 1];
int pivotCol[MAX_COUNTERS];
int isPivot[MAX_BUTTONS];
int pivotRows[MAX_BUTTONS];
int freeVars[MAX_BUTTONS];
int numFree;
int maxPresses[MAX_BUTTONS];
int freeValues[MAX_BUTTONS];
int bestResult;

void parseLine(NSString *line) {
    numCounters = 0; numButtons = 0;
    NSUInteger p = 0;
    while (p < line.length) {
        unichar c = [line characterAtIndex:p];
        if (c == '(') {
            p++;
            int btnIdx = numButtons++;
            btnSize[btnIdx] = 0;
            while (p < line.length && [line characterAtIndex:p] != ')') {
                int x = 0;
                while (p < line.length && isdigit([line characterAtIndex:p])) {
                    x = x * 10 + ([line characterAtIndex:p] - '0');
                    p++;
                }
                buttons[btnIdx][btnSize[btnIdx]++] = x;
                if (p < line.length && [line characterAtIndex:p] == ',') p++;
            }
            if (p < line.length && [line characterAtIndex:p] == ')') p++;
        } else if (c == '{') {
            p++;
            while (p < line.length && [line characterAtIndex:p] != '}') {
                int x = 0;
                while (p < line.length && isdigit([line characterAtIndex:p])) {
                    x = x * 10 + ([line characterAtIndex:p] - '0');
                    p++;
                }
                targets[numCounters++] = x;
                if (p < line.length && [line characterAtIndex:p] == ',') p++;
            }
            break;
        } else p++;
    }
}

void gauss() {
    for (int j = 0; j < numCounters; j++) {
        for (int i = 0; i <= numButtons; i++) matrix[j][i] = 0.0;
        matrix[j][numButtons] = (double)targets[j];
    }
    for (int i = 0; i < numButtons; i++)
        for (int j = 0; j < btnSize[i]; j++) {
            int c = buttons[i][j];
            if (c < numCounters) matrix[c][i] = 1.0;
        }

    for (int i = 0; i < numCounters; i++) pivotCol[i] = -1;

    int row = 0;
    for (int col = 0; col < numButtons && row < numCounters; col++) {
        int maxRow = row;
        for (int r = row + 1; r < numCounters; r++)
            if (fabs(matrix[r][col]) > fabs(matrix[maxRow][col])) maxRow = r;
        if (fabs(matrix[maxRow][col]) < 1e-9) continue;

        for (int c = 0; c <= numButtons; c++) {
            double tmp = matrix[row][c];
            matrix[row][c] = matrix[maxRow][c];
            matrix[maxRow][c] = tmp;
        }
        double scale = matrix[row][col];
        for (int c = col; c <= numButtons; c++) matrix[row][c] /= scale;

        for (int r = 0; r < numCounters; r++) {
            if (r != row && fabs(matrix[r][col]) > 1e-9) {
                double factor = matrix[r][col];
                for (int c = col; c <= numButtons; c++)
                    matrix[r][c] -= factor * matrix[row][c];
            }
        }
        pivotCol[row] = col;
        row++;
    }

    int rank = row;
    for (int i = 0; i < numButtons; i++) {
        isPivot[i] = 0;
        pivotRows[i] = -1;
    }
    for (int r = 0; r < rank; r++) {
        int c = pivotCol[r];
        if (c >= 0) {
            isPivot[c] = 1;
            pivotRows[c] = r;
        }
    }
    numFree = 0;
    for (int i = 0; i < numButtons; i++)
        if (!isPivot[i]) freeVars[numFree++] = i;

    for (int i = 0; i < numButtons; i++) {
        maxPresses[i] = INF;
        for (int j = 0; j < btnSize[i]; j++) {
            int c = buttons[i][j];
            if (c < numCounters && targets[c] < maxPresses[i]) maxPresses[i] = targets[c];
        }
        if (maxPresses[i] == INF) maxPresses[i] = 0;
    }

    for (int i = 0; i < numFree; i++)
        for (int j = i + 1; j < numFree; j++)
            if (maxPresses[freeVars[i]] > maxPresses[freeVars[j]]) {
                int t = freeVars[i];
                freeVars[i] = freeVars[j];
                freeVars[j] = t;
            }
}

int computePivots(int *presses) {
    for (int i = 0; i < numButtons; i++) presses[i] = 0;

    for (int i = 0; i < numFree; i++) presses[freeVars[i]] = freeValues[i];

    for (int r = numCounters - 1; r >= 0; r--) {
        int col = pivotCol[r];
        if (col < 0) continue;
        double val = matrix[r][numButtons];
        for (int c = col + 1; c < numButtons; c++)
            val -= matrix[r][c] * (double)presses[c];

        int intVal = (int)round(val);
        if (fabs(val - (double)intVal) > 1e-6) return 0;
        if (intVal < 0) return 0;
        if (intVal > maxPresses[col]) return 0;
        presses[col] = intVal;
    }

    int sum = 0;
    for (int i = 0; i < numButtons; i++) sum += presses[i];
    return sum;
}

void enumerate(int idx, int currentSum) {
    if (currentSum >= bestResult) return;
    if (idx == numFree) {
        int presses[MAX_BUTTONS];
        int sum = computePivots(presses);
        if (sum && sum < bestResult) bestResult = sum;
        return;
    }
    int fv = freeVars[idx];
    int maxVal = maxPresses[fv];
    for (int v = 0; v <= maxVal; v++) {
        freeValues[idx] = v;
        enumerate(idx + 1, currentSum + v);
    }
}

int solve() {
    gauss();
    bestResult = INF;
    enumerate(0, 0);
    return (bestResult == INF) ? -1 : bestResult;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *content = [NSString stringWithContentsOfFile:@"input.txt"
                                                      encoding:NSUTF8StringEncoding
                                                         error:nil];
        if (!content) return 0;
        NSArray *lines = [content componentsSeparatedByString:@"\n"];
        int total = 0;
        for (NSString *line in lines) {
            NSString *trimmed = [line stringByTrimmingCharactersInSet:
                                [NSCharacterSet whitespaceAndNewlineCharacterSet]];
            if (trimmed.length == 0) continue;
            parseLine(trimmed);
            int res = solve();
            if (res > 0) total += res;
        }
        printf("%d\n", total);
    }
    return 0;
}
