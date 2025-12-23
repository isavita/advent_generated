
#import <Foundation/Foundation.h>

#define MAX_NODES 17576
#define MAX_INSTRUCTIONS 512
#define MAX_LINE_LEN 256

static inline int nameToIndex(const char *n) {
    return (n[0]-'A')*26*26 + (n[1]-'A')*26 + (n[2]-'A');
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        FILE *f = fopen("input.txt", "r");
        if (!f) return 1;

        char instr[MAX_INSTRUCTIONS];
        if (!fgets(instr, MAX_INSTRUCTIONS, f)) { fclose(f); return 1; }
        instr[strcspn(instr, "\n")] = 0;
        size_t instrLen = strlen(instr);
        if (instrLen == 0) { fclose(f); return 1; }

        char line[MAX_LINE_LEN];
        fgets(line, MAX_LINE_LEN, f); // skip blank

        static int left[MAX_NODES];
        static int right[MAX_NODES];
        for (int i=0;i<MAX_NODES;i++) left[i]=right[i]=-1;

        while (fgets(line, MAX_LINE_LEN, f)) {
            if (strlen(line)<10) continue;
            char node[4], l[4], r[4];
            if (sscanf(line, "%3s = (%3s, %3s)", node, l, r)==3) {
                int idx=nameToIndex(node);
                left[idx]=nameToIndex(l);
                right[idx]=nameToIndex(r);
            }
        }
        fclose(f);

        int cur=nameToIndex("AAA");
        int target=nameToIndex("ZZZ");
        unsigned long long steps=0;

        while (cur!=target) {
            char d=instr[steps%instrLen];
            cur = (d=='L') ? left[cur] : right[cur];
            if (cur<0||cur>=MAX_NODES) return 1;
            steps++;
            if (steps>1000000000ULL) return 1;
        }
        printf("%llu\n", steps);
    }
    return 0;
}
