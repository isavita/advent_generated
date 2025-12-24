
#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        FILE *file = fopen("input.txt", "r");
        if (!file) return 1;
        char line1[64], line2[64];
        fgets(line1, sizeof(line1), file);
        fgets(line2, sizeof(line2), file);
        fclose(file);
        int p1 = atoi(&line1[28]);
        int p2 = atoi(&line2[28]);
        int pos1 = p1, pos2 = p2, score1 = 0, score2 = 0, die = 1, rolls = 0;
        while (1) {
            int move = (die%100) + ((die+1)%100) + ((die+2)%100);
            die += 3; rolls += 3;
            pos1 = (pos1 + move - 1) % 10 + 1;
            score1 += pos1;
            if (score1 >= 1000) { printf("%d\n", score2 * rolls); break; }
            move = (die%100) + ((die+1)%100) + ((die+2)%100);
            die += 3; rolls += 3;
            pos2 = (pos2 + move - 1) % 10 + 1;
            score2 += pos2;
            if (score2 >= 1000) { printf("%d\n", score1 * rolls); break; }
        }
    }
    return 0;
}
