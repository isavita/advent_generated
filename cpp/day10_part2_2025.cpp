#include <iostream>
#include <fstream>
#include <string>
#include <cmath>
#include <algorithm>

using namespace std;

const int MAX_LINE = 1024;
const int MAX_COUNTERS = 20;
const int MAX_BUTTONS = 50;
const int INF = 0x3f3f3f3f;

int buttons[MAX_BUTTONS][MAX_COUNTERS];
int btnSize[MAX_BUTTONS];
int targets[MAX_COUNTERS];
int numCounters, numButtons;

double matrixA[MAX_COUNTERS][MAX_BUTTONS + 1];
int pivotCol[MAX_COUNTERS];
int isPivot[MAX_BUTTONS];
int pivotRows[MAX_BUTTONS];
int freeVars[MAX_BUTTONS];
int numFree;
int maxPresses[MAX_BUTTONS];
int freeValues[MAX_BUTTONS];
int bestResult;

void parseLine(const string &line) {
    numCounters = 0;
    numButtons = 0;
    size_t p = 0;
    while (p < line.size()) {
        char ch = line[p];
        if (ch == '(') {
            p++;
            int btnIdx = numButtons++;
            btnSize[btnIdx] = 0;
            while (p < line.size() && line[p] != ')') {
                int x = 0;
                while (p < line.size() && line[p] >= '0' && line[p] <= '9') {
                    x = x * 10 + (line[p] - '0');
                    p++;
                }
                buttons[btnIdx][btnSize[btnIdx]++] = x;
                if (p < line.size() && line[p] == ',') p++;
            }
            if (p < line.size() && line[p] == ')') p++;
        } else if (ch == '{') {
            p++;
            while (p < line.size() && line[p] != '}') {
                int x = 0;
                while (p < line.size() && line[p] >= '0' && line[p] <= '9') {
                    x = x * 10 + (line[p] - '0');
                    p++;
                }
                targets[numCounters++] = x;
                if (p < line.size() && line[p] == ',') p++;
            }
            break;
        } else {
            p++;
        }
    }
}

void gauss() {
    for (int j = 0; j < numCounters; ++j) {
        for (int i = 0; i <= numButtons; ++i) matrixA[j][i] = 0.0;
        matrixA[j][numButtons] = (double)targets[j];
    }
    for (int i = 0; i < numButtons; ++i) {
        for (int j = 0; j < btnSize[i]; ++j) {
            int c = buttons[i][j];
            if (c < numCounters) matrixA[c][i] = 1.0;
        }
    }

    for (int i = 0; i < numCounters; ++i) pivotCol[i] = -1;

    int row = 0;
    for (int col = 0; col < numButtons && row < numCounters; ++col) {
        int maxRow = row;
        for (int r = row + 1; r < numCounters; ++r)
            if (fabs(matrixA[r][col]) > fabs(matrixA[maxRow][col])) maxRow = r;
        if (fabs(matrixA[maxRow][col]) < 1e-9) continue;

        for (int c = 0; c <= numButtons; ++c) {
            double tmp = matrixA[row][c];
            matrixA[row][c] = matrixA[maxRow][c];
            matrixA[maxRow][c] = tmp;
        }
        double scale = matrixA[row][col];
        for (int c = col; c <= numButtons; ++c) matrixA[row][c] /= scale;

        for (int r = 0; r < numCounters; ++r) {
            if (r != row && fabs(matrixA[r][col]) > 1e-9) {
                double factor = matrixA[r][col];
                for (int c = col; c <= numButtons; ++c)
                    matrixA[r][c] -= factor * matrixA[row][c];
            }
        }
        pivotCol[row] = col;
        ++row;
    }

    int rank = row;
    for (int i = 0; i < numButtons; ++i) {
        isPivot[i] = 0;
        pivotRows[i] = -1;
    }
    for (int r = 0; r < rank; ++r) {
        int c = pivotCol[r];
        if (c >= 0) {
            isPivot[c] = 1;
            pivotRows[c] = r;
        }
    }
    numFree = 0;
    for (int i = 0; i < numButtons; ++i)
        if (!isPivot[i]) freeVars[numFree++] = i;

    for (int i = 0; i < numButtons; ++i) {
        maxPresses[i] = INF;
        for (int j = 0; j < btnSize[i]; ++j) {
            int c = buttons[i][j];
            if (c < numCounters && targets[c] < maxPresses[i]) maxPresses[i] = targets[c];
        }
        if (maxPresses[i] == INF) maxPresses[i] = 0;
    }

    for (int i = 0; i < numFree; ++i)
        for (int j = i + 1; j < numFree; ++j)
            if (maxPresses[freeVars[i]] > maxPresses[freeVars[j]]) {
                int t = freeVars[i];
                freeVars[i] = freeVars[j];
                freeVars[j] = t;
            }
}

int computePivots(int presses[]) {
    for (int i = 0; i < numButtons; ++i) presses[i] = 0;
    for (int i = 0; i < numFree; ++i) presses[freeVars[i]] = freeValues[i];

    for (int r = numCounters - 1; r >= 0; --r) {
        int col = pivotCol[r];
        if (col < 0) continue;
        double val = matrixA[r][numButtons];
        for (int c = col + 1; c < numButtons; ++c)
            val -= matrixA[r][c] * (double)presses[c];

        int intVal = (int)round(val);
        if (fabs(val - (double)intVal) > 1e-6) return 0;
        if (intVal < 0) return 0;
        if (intVal > maxPresses[col]) return 0;
        presses[col] = intVal;
    }

    int sum = 0;
    for (int i = 0; i < numButtons; ++i) sum += presses[i];
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
    for (int v = 0; v <= maxVal; ++v) {
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

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    ifstream fin("input.txt");
    if (!fin) return 0;

    string line;
    long long total = 0;
    while (getline(fin, line)) {
        if (line.empty()) continue;
        parseLine(line);
        int res = solve();
        if (res > 0) total += res;
    }

    cout << total << "\n";
    return 0;
}