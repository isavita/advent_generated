
#import <Foundation/Foundation.h>

#define MEMO_SIZE 65536
#define MAX_ROBOTS 26

const char *key_pad[] = {"789", "456", "123", " 0A"};
const int key_pad_rows = 4;
const int key_pad_cols = 3;

const char *robot_pad[] = {" ^A", "<v>"};
const int robot_pad_rows = 2;
const int robot_pad_cols = 3;

typedef struct {
    char *code;
    int robots;
    long long value;
    int occupied;
} MemoEntry;

static MemoEntry memo_map[MEMO_SIZE];

unsigned int hash(const char *str, int num) {
    unsigned int h = 5381;
    int c;
    while ((c = *str++))
        h = ((h << 5) + h) + c;
    h = ((h << 5) + h) ^ num;
    return h;
}

void find_position(const char **mat, int rows, char ch, int *r, int *c) {
    for (int i = 0; i < rows; i++) {
        const char *p = strchr(mat[i], ch);
        if (p) {
            *r = i;
            *c = (int)(p - mat[i]);
            return;
        }
    }
    *r = -1;
    *c = -1;
}

int is_ok(const char **mat, int rows, int cols, int r, int c, const char *seq) {
    for (int i = 0; seq[i]; i++) {
        if (r < 0 || r >= rows || c < 0 || c >= cols || mat[r][c] == ' ') {
            return 0;
        }
        switch (seq[i]) {
            case '^': r--; break;
            case 'v': r++; break;
            case '<': c--; break;
            case '>': c++; break;
        }
    }
    return 1;
}

void generate_moves(char *buffer, int r, int c, char objective,
                    const char **pad, int rows, int cols) {
    int obj_r, obj_c;
    find_position(pad, rows, objective, &obj_r, &obj_c);

    char *p = buffer;
    if (c > obj_c) for (int i = 0; i < c - obj_c; i++) *p++ = '<';
    if (r > obj_r) for (int i = 0; i < r - obj_r; i++) *p++ = '^';
    if (r < obj_r) for (int i = 0; i < obj_r - r; i++) *p++ = 'v';
    if (c < obj_c) for (int i = 0; i < obj_c - c; i++) *p++ = '>';
    *p = '\0';

    if (!is_ok(pad, rows, cols, r, c, buffer)) {
        p = buffer;
        if (c < obj_c) for (int i = 0; i < obj_c - c; i++) *p++ = '>';
        if (r > obj_r) for (int i = 0; i < r - obj_r; i++) *p++ = '^';
        if (r < obj_r) for (int i = 0; i < obj_r - r; i++) *p++ = 'v';
        if (c > obj_c) for (int i = 0; i < c - obj_c; i++) *p++ = '<';
        *p = '\0';
    }
}

long long solve(const char *code, int robots) {
    unsigned int index = hash(code, robots) & (MEMO_SIZE - 1);
    unsigned int start_index = index;
    do {
        if (memo_map[index].occupied && memo_map[index].robots == robots && strcmp(memo_map[index].code, code) == 0) {
            return memo_map[index].value;
        }
        if (!memo_map[index].occupied) break;
        index = (index + 1) & (MEMO_SIZE - 1);
    } while(index != start_index);

    if (robots <= 0) {
        return strlen(code);
    }

    long long ret = 0;
    int current_r, current_c;
    const char **pad;
    int pad_rows, pad_cols;

    if (robots == MAX_ROBOTS) {
        current_r = 3; current_c = 2;
        pad = key_pad;
        pad_rows = key_pad_rows;
        pad_cols = key_pad_cols;
    } else {
        current_r = 0; current_c = 2;
        pad = robot_pad;
        pad_rows = robot_pad_rows;
        pad_cols = robot_pad_cols;
    }

    char moves[128];
    for (size_t i = 0; i < strlen(code); i++) {
        char ch = code[i];
        char *moves_ptr = moves;
        generate_moves(moves_ptr, current_r, current_c, ch, pad, pad_rows, pad_cols);
        find_position(pad, pad_rows, ch, &current_r, &current_c);
        strcat(moves, "A");
        ret += solve(moves, robots - 1);
    }

    index = hash(code, robots) & (MEMO_SIZE - 1);
    while (memo_map[index].occupied) {
        index = (index + 1) & (MEMO_SIZE - 1);
    }
    memo_map[index].code = strdup(code);
    memo_map[index].robots = robots;
    memo_map[index].value = ret;
    memo_map[index].occupied = 1;
    return ret;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *fileContent = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        if (!fileContent) return 1;

        long long total_ret = 0;
        NSArray *lines = [fileContent componentsSeparatedByString:@"\n"];
        for (NSString *line in lines) {
            NSString *trimmed = [line stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceAndNewlineCharacterSet]];
            if ([trimmed length] == 0) continue;

            long long numeric_part = 0;
            for (NSUInteger i = 0; i < [trimmed length]; i++) {
                unichar ch = [trimmed characterAtIndex:i];
                if (isdigit(ch)) {
                    numeric_part = numeric_part * 10 + (ch - '0');
                }
            }

            if (numeric_part > 0) {
                total_ret += solve([trimmed UTF8String], MAX_ROBOTS) * numeric_part;
            }
        }

        printf("%lld\n", total_ret);

        for (int i = 0; i < MEMO_SIZE; i++) {
            if (memo_map[i].occupied) {
                free(memo_map[i].code);
            }
        }
    }
    return 0;
}
