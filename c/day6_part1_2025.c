
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define MAXLINES 10000
#define MAXLEN 10000

char lines[MAXLINES][MAXLEN];
int nlines = 0;
int maxw = 0;

int is_sep(int col) {
    for (int i = 0; i < nlines; i++)
        if (col < strlen(lines[i]) && !isspace(lines[i][col]))
            return 0;
    return 1;
}

void bignum_add(char *res, const char *a, const char *b) {
    int al = strlen(a), bl = strlen(b), carry = 0, i = 0, j = 0, k = 0;
    char tmp[10000] = {0};
    while (al || bl || carry) {
        int da = al ? a[--al] - '0' : 0;
        int db = bl ? b[--bl] - '0' : 0;
        int sum = da + db + carry;
        carry = sum / 10;
        tmp[k++] = sum % 10 + '0';
    }
    while (k > 1 && tmp[k - 1] == '0') k--;
    for (int l = 0; l < k; l++) res[l] = tmp[k - 1 - l];
    res[k] = '\0';
}

void bignum_mul(char *res, const char *a, const char *b) {
    int al = strlen(a), bl = strlen(b);
    int prod[20000] = {0};
    for (int i = al - 1; i >= 0; i--)
        for (int j = bl - 1; j >= 0; j--)
            prod[i + j + 1] += (a[i] - '0') * (b[j] - '0');
    for (int i = al + bl - 1; i > 0; i--)
        if (prod[i] > 9) {
            prod[i - 1] += prod[i] / 10;
            prod[i] %= 10;
        }
    int start = 0;
    while (start < al + bl && prod[start] == 0) start++;
    int k = 0;
    for (int i = start; i < al + bl; i++) res[k++] = prod[i] + '0';
    res[k] = '\0';
}

void process_block(int sc, int ec, char *gt) {
    char nums[1000][1000];
    int nn = 0, op = 0;
    for (int i = 0; i < nlines; i++) {
        int e = ec + 1;
        if (e > (int)strlen(lines[i])) e = strlen(lines[i]);
        if (sc >= (int)strlen(lines[i])) continue;
        char seg[1000], tmp[1000];
        int len = e - sc;
        strncpy(seg, lines[i] + sc, len);
        seg[len] = '\0';
        int l = 0, r = strlen(seg) - 1;
        while (l <= r && isspace(seg[l])) l++;
        while (r >= l && isspace(seg[r])) r--;
        if (l > r) continue;
        int m = 0;
        for (int p = l; p <= r; p++) tmp[m++] = seg[p];
        tmp[m] = '\0';
        if (strcmp(tmp, "+") == 0) op = 1;
        else if (strcmp(tmp, "*") == 0) op = 2;
        else {
            strcpy(nums[nn], tmp);
            nn++;
        }
    }
    if (nn == 0) return;
    char acc[1000] = "0";
    if (op == 1) {
        for (int i = 0; i < nn; i++) {
            char t[1000];
            strcpy(t, acc);
            bignum_add(acc, t, nums[i]);
        }
    } else if (op == 2) {
        strcpy(acc, "1");
        for (int i = 0; i < nn; i++) {
            char t[1000];
            strcpy(t, acc);
            bignum_mul(acc, t, nums[i]);
        }
    } else if (nn == 1) {
        strcpy(acc, nums[0]);
    }
    char t[1000];
    strcpy(t, gt);
    bignum_add(gt, t, acc);
}

int main() {
    FILE *f = fopen("input.txt", "r");
    while (fgets(lines[nlines], MAXLEN, f)) {
        lines[nlines][strcspn(lines[nlines], "\n")] = '\0';
        int l = strlen(lines[nlines]);
        if (l > maxw) maxw = l;
        nlines++;
    }
    fclose(f);
    char grand[10000] = "0";
    int inb = 0, sc = 0;
    for (int x = 0; x < maxw; x++) {
        if (!is_sep(x)) {
            if (!inb) { inb = 1; sc = x; }
        } else {
            if (inb) { process_block(sc, x - 1, grand); inb = 0; }
        }
    }
    if (inb) process_block(sc, maxw - 1, grand);
    printf("Grand total: %s\n", grand);
    return 0;
}
