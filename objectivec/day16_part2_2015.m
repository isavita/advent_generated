
#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        FILE *file = fopen("input.txt", "r");
        if (!file) return 1;

        int target[10] = {3,7,2,3,0,0,5,3,2,1};
        char line[256], thing1[20], thing2[20], thing3[20];
        int sueNum, amount1, amount2, amount3;

        while (fgets(line, sizeof(line), file)) {
            if (sscanf(line, "Sue %d: %19s %d, %19s %d, %19s %d",
                       &sueNum, thing1, &amount1, thing2, &amount2, thing3, &amount3) != 7) continue;

            thing1[strlen(thing1)-1] = '\0';
            thing2[strlen(thing2)-1] = '\0';
            thing3[strlen(thing3)-1] = '\0';

            const char *things[3] = {thing1, thing2, thing3};
            int counts[3] = {amount1, amount2, amount3};
            int indexes[3];
            for (int i = 0; i < 3; ++i) {
                if (strcmp(things[i], "children") == 0) indexes[i] = 0;
                else if (strcmp(things[i], "cats") == 0) indexes[i] = 1;
                else if (strcmp(things[i], "samoyeds") == 0) indexes[i] = 2;
                else if (strcmp(things[i], "pomeranians") == 0) indexes[i] = 3;
                else if (strcmp(things[i], "akitas") == 0) indexes[i] = 4;
                else if (strcmp(things[i], "vizslas") == 0) indexes[i] = 5;
                else if (strcmp(things[i], "goldfish") == 0) indexes[i] = 6;
                else if (strcmp(things[i], "trees") == 0) indexes[i] = 7;
                else if (strcmp(things[i], "cars") == 0) indexes[i] = 8;
                else if (strcmp(things[i], "perfumes") == 0) indexes[i] = 9;
                else indexes[i] = -1;
            }

            int ok = 1;
            for (int i = 0; i < 3; ++i) {
                if (indexes[i] == -1) continue;
                if (indexes[i] == 1 || indexes[i] == 7) {
                    if (counts[i] <= target[indexes[i]]) { ok = 0; break; }
                } else if (indexes[i] == 3 || indexes[i] == 6) {
                    if (counts[i] >= target[indexes[i]]) { ok = 0; break; }
                } else {
                    if (counts[i] != target[indexes[i]]) { ok = 0; break; }
                }
            }

            if (ok) {
                printf("%d\n", sueNum);
                fclose(file);
                return 0;
            }
        }
        fclose(file);
        return 1;
    }
}
