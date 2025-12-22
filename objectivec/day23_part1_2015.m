
#import <Foundation/Foundation.h>

typedef struct {
    char instr[4];
    char reg;
    int offset;
} Instr;

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        FILE *fp = fopen("input.txt", "r");
        if (!fp) return 1;
        Instr prog[200];
        int count = 0;
        char line[128];
        while (fgets(line, sizeof(line), fp)) {
            char *tok = strtok(line, " ,\n");
            strcpy(prog[count].instr, tok);
            if (strcmp(tok, "hlf") == 0 || strcmp(tok, "tpl") == 0 || strcmp(tok, "inc") == 0) {
                tok = strtok(NULL, " ,\n");
                prog[count].reg = tok[0];
                prog[count].offset = 0;
            } else if (strcmp(tok, "jmp") == 0) {
                tok = strtok(NULL, " ,\n");
                prog[count].reg = ' ';
                prog[count].offset = atoi(tok);
            } else {
                tok = strtok(NULL, " ,\n");
                prog[count].reg = tok[0];
                tok = strtok(NULL, " ,\n");
                prog[count].offset = atoi(tok);
            }
            count++;
        }
        fclose(fp);
        int a = 0, b = 0, pc = 0;
        while (pc >= 0 && pc < count) {
            Instr cur = prog[pc];
            if (strcmp(cur.instr, "hlf") == 0) {
                if (cur.reg == 'a') a /= 2; else b /= 2;
                pc++;
            } else if (strcmp(cur.instr, "tpl") == 0) {
                if (cur.reg == 'a') a *= 3; else b *= 3;
                pc++;
            } else if (strcmp(cur.instr, "inc") == 0) {
                if (cur.reg == 'a') a++; else b++;
                pc++;
            } else if (strcmp(cur.instr, "jmp") == 0) {
                pc += cur.offset;
            } else if (strcmp(cur.instr, "jie") == 0) {
                int v = (cur.reg == 'a' ? a : b);
                pc = (v % 2 == 0) ? pc + cur.offset : pc + 1;
            } else { // jio
                int v = (cur.reg == 'a' ? a : b);
                pc = (v == 1) ? pc + cur.offset : pc + 1;
            }
        }
        printf("%d\n", b);
    }
    return 0;
}
