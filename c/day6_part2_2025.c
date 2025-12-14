
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

char *add_str(const char *a, const char *b) {
    size_t la = strlen(a), lb = strlen(b);
    size_t l = la > lb ? la : lb;
    char *res = malloc(l + 2);
    size_t i = 0, carry = 0;
    while (i < la || i < lb || carry) {
        int da = i < la ? a[la - 1 - i] - '0' : 0;
        int db = i < lb ? b[lb - 1 - i] - '0' : 0;
        int sum = da + db + carry;
        res[l - i] = (sum % 10) + '0';
        carry = sum / 10;
        i++;
    }
    size_t start = l + 1 - i;
    memmove(res, res + start, i + 1);
    return res;
}

char *mul_str(const char *a, const char *b) {
    size_t la = strlen(a), lb = strlen(b);
    if ((la == 1 && a[0] == '0') || (lb == 1 && b[0] == '0')) {
        char *z = malloc(2);
        strcpy(z, "0");
        return z;
    }
    int *tmp = calloc(la + lb, sizeof(int));
    for (size_t i = 0; i < la; ++i) {
        int da = a[la - 1 - i] - '0';
        for (size_t j = 0; j < lb; ++j) {
            int db = b[lb - 1 - j] - '0';
            tmp[i + j] += da * db;
        }
    }
    int carry = 0;
    for (size_t k = 0; k < la + lb; ++k) {
        int sum = tmp[k] + carry;
        tmp[k] = sum % 10;
        carry = sum / 10;
    }
    size_t len = la + lb;
    while (len > 1 && tmp[len - 1] == 0) len--;
    char *res = malloc(len + 1);
    for (size_t i = 0; i < len; ++i)
        res[i] = tmp[len - 1 - i] + '0';
    res[len] = '\0';
    free(tmp);
    return res;
}

/* process a vertical block [start,end] (inclusive) */
void process_block(char **lines, size_t linecnt, int start, int end, char **grand_total) {
    char **nums = NULL;
    size_t ncnt = 0;
    char op = '+';
    for (int c = start; c <= end; ++c) {
        char *buf = malloc(linecnt + 1);
        size_t pos = 0;
        for (size_t r = 0; r < linecnt; ++r) {
            char *ln = lines[r];
            if ((int)strlen(ln) > c) {
                char ch = ln[c];
                if (isdigit((unsigned char)ch))
                    buf[pos++] = ch;
                else if (ch == '+' || ch == '*')
                    op = ch;
            }
        }
        if (pos) {
            buf[pos] = '\0';
            nums = realloc(nums, (ncnt + 1) * sizeof(char *));
            nums[ncnt++] = buf;
        } else {
            free(buf);
        }
    }
    if (ncnt == 0) return;
    char *block_res = NULL;
    if (op == '*') {
        block_res = malloc(2);
        strcpy(block_res, "1");
        for (size_t i = 0; i < ncnt; ++i) {
            char *tmp = mul_str(block_res, nums[i]);
            free(block_res);
            block_res = tmp;
        }
    } else {
        block_res = malloc(2);
        strcpy(block_res, "0");
        for (size_t i = 0; i < ncnt; ++i) {
            char *tmp = add_str(block_res, nums[i]);
            free(block_res);
            block_res = tmp;
        }
    }
    char *new_total = add_str(*grand_total, block_res);
    free(*grand_total);
    free(block_res);
    for (size_t i = 0; i < ncnt; ++i) free(nums[i]);
    free(nums);
    *grand_total = new_total;
}

int main(void) {
    FILE *f = fopen("input.txt", "r");
    if (!f) return 1;
    size_t capacity = 1024;
    char **lines = malloc(capacity * sizeof(char *));
    size_t linecnt = 0;
    size_t maxw = 0;
    char linebuf[4096];
    while (fgets(linebuf, sizeof(linebuf), f)) {
        size_t len = strlen(linebuf);
        while (len && (linebuf[len - 1] == '\n' || linebuf[len - 1] == '\r')) linebuf[--len] = '\0';
        if (linecnt == capacity) {
            capacity *= 2;
            lines = realloc(lines, capacity * sizeof(char *));
        }
        lines[linecnt] = malloc(len + 1);
        strcpy(lines[linecnt], linebuf);
        if (len > maxw) maxw = len;
        linecnt++;
    }
    fclose(f);
    if (linecnt == 0) {
        printf("Grand total: 0\n");
        return 0;
    }
    int *is_sep = calloc(maxw, sizeof(int));
    for (size_t x = 0; x < maxw; ++x) {
        int allspace = 1;
        for (size_t r = 0; r < linecnt; ++r) {
            if (x < strlen(lines[r]) && !isspace((unsigned char)lines[r][x])) {
                allspace = 0;
                break;
            }
        }
        is_sep[x] = allspace;
    }
    char *grand_total = malloc(2);
    strcpy(grand_total, "0");
    int in_block = 0, start = 0;
    for (size_t x = 0; x < maxw; ++x) {
        if (!is_sep[x]) {
            if (!in_block) {
                in_block = 1;
                start = (int)x;
            }
        } else {
            if (in_block) {
                process_block(lines, linecnt, start, (int)x - 1, &grand_total);
                in_block = 0;
            }
        }
    }
    if (in_block) process_block(lines, linecnt, start, (int)maxw - 1, &grand_total);
    printf("Grand total: %s\n", grand_total);
    free(grand_total);
    free(is_sep);
    for (size_t i = 0; i < linecnt; ++i) free(lines[i]);
    free(lines);
    return 0;
}
