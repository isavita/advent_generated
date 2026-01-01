
import math, sys

INF = 10**9
TOL = 1e-6

def parse_line(s):
    i = 0
    btns = []
    tgt = []
    while i < len(s):
        if s[i] == '(':
            i += 1
            cur = []
            while i < len(s) and s[i] != ')':
                num = 0
                while i < len(s) and s[i].isdigit():
                    num = num * 10 + ord(s[i]) - 48
                    i += 1
                cur.append(num)
                if i < len(s) and s[i] == ',':
                    i += 1
            i += 1
            btns.append(cur)
        elif s[i] == '{':
            i += 1
            while i < len(s) and s[i] != '}':
                num = 0
                while i < len(s) and s[i].isdigit():
                    num = num * 10 + ord(s[i]) - 48
                    i += 1
                tgt.append(num)
                if i < len(s) and s[i] == ',':
                    i += 1
            break
        else:
            i += 1
    return btns, tgt

class Solver:
    def __init__(self, buttons, targets):
        self.buttons = buttons
        self.targets = targets
        self.nC = len(targets)
        self.nB = len(buttons)
        self.mat = [[0.0] * (self.nB + 1) for _ in range(self.nC)]
        self.pivot_col = [-1] * self.nC
        self.is_pivot = [False] * self.nB
        self.pivot_row = [-1] * self.nB
        self.free_vars = []
        self.max_press = [0] * self.nB
        self.free_vals = []
        self.best = INF

    def gauss(self):
        for r in range(self.nC):
            for c in range(self.nB + 1):
                self.mat[r][c] = 0.0
            self.mat[r][self.nB] = float(self.targets[r])
        for b, lst in enumerate(self.buttons):
            for c in lst:
                if c < self.nC:
                    self.mat[c][b] = 1.0
        row = 0
        for col in range(self.nB):
            if row >= self.nC:
                break
            sel = max(range(row, self.nC), key=lambda r: abs(self.mat[r][col]))
            if abs(self.mat[sel][col]) < 1e-9:
                continue
            self.mat[row], self.mat[sel] = self.mat[sel], self.mat[row]
            piv = self.mat[row][col]
            for c in range(col, self.nB + 1):
                self.mat[row][c] /= piv
            for r in range(self.nC):
                if r != row and abs(self.mat[r][col]) > 1e-9:
                    fac = self.mat[r][col]
                    for c in range(col, self.nB + 1):
                        self.mat[r][c] -= fac * self.mat[row][c]
            self.pivot_col[row] = col
            row += 1
        rank = row
        for i in range(self.nB):
            self.is_pivot[i] = False
            self.pivot_row[i] = -1
        for r in range(rank):
            c = self.pivot_col[r]
            if c >= 0:
                self.is_pivot[c] = True
                self.pivot_row[c] = r
        self.free_vars = [i for i in range(self.nB) if not self.is_pivot[i]]
        for i, b in enumerate(self.buttons):
            lim = INF
            for c in b:
                if c < self.nC and self.targets[c] < lim:
                    lim = self.targets[c]
            self.max_press[i] = 0 if lim == INF else lim
        self.free_vars.sort(key=lambda x: self.max_press[x])
        self.free_vals = [0] * len(self.free_vars)

    def compute(self):
        presses = [0] * self.nB
        for i, v in enumerate(self.free_vals):
            presses[self.free_vars[i]] = v
        for r in range(self.nC - 1, -1, -1):
            c = self.pivot_col[r]
            if c < 0:
                continue
            v = self.mat[r][self.nB]
            for j in range(c + 1, self.nB):
                v -= self.mat[r][j] * presses[j]
            iv = int(round(v))
            if abs(v - iv) > TOL or iv < 0 or iv > self.max_press[c]:
                return 0
            presses[c] = iv
        return sum(presses)

    def dfs(self, idx, cur):
        if cur >= self.best:
            return
        if idx == len(self.free_vars):
            s = self.compute()
            if 0 < s < self.best:
                self.best = s
            return
        fv = self.free_vars[idx]
        for v in range(self.max_press[fv] + 1):
            self.free_vals[idx] = v
            self.dfs(idx + 1, cur + v)

    def solve(self):
        self.gauss()
        self.dfs(0, 0)
        return -1 if self.best == INF else self.best

def main():
    total = 0
    try:
        lines = open("input.txt").read().splitlines()
    except:
        return
    for raw in lines:
        line = raw.strip()
        if not line:
            continue
        btns, tgts = parse_line(line)
        if not btns or not tgts:
            continue
        res = Solver(btns, tgts).solve()
        if res > 0:
            total += res
    print(total)

if __name__ == "__main__":
    main()
