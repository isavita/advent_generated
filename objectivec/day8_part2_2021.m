
#import <Foundation/Foundation.h>

int compare_chars(const void *a, const void *b) {
    return (*(char*)a - *(char*)b);
}

void sort_string(char *str) {
    qsort(str, strlen(str), sizeof(char), compare_chars);
}

bool contains_all_chars_sorted(const char* larger_sorted, const char* smaller_sorted) {
    while (*smaller_sorted && *larger_sorted) {
        if (*smaller_sorted == *larger_sorted) {
            smaller_sorted++;
            larger_sorted++;
        } else if (*larger_sorted < *smaller_sorted) {
            larger_sorted++;
        } else {
            return false;
        }
    }
    return (*smaller_sorted == '\0');
}

int main(int argc, char *argv[]) {
    NSString *filePath = @"input.txt";
    NSError *error = nil;
    NSString *content = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:&error];
    if (error) {
        NSLog(@"Error reading file: %@", error.localizedDescription);
        return 1;
    }

    NSArray *lines = [content componentsSeparatedByString:@"\n"];
    long long total_sum = 0;

    for (NSString *line in lines) {
        if ([line isEqualToString:@""]) continue;

        NSArray *components = [line componentsSeparatedByString:@" | "];
        if (components.count != 2) continue;

        NSArray *uniquePatterns = [[components objectAtIndex:0] componentsSeparatedByString:@" "];
        NSArray *outputPatterns = [[components objectAtIndex:1] componentsSeparatedByString:@" "];

        char unique_patterns[10][8];
        char output_patterns[4][8];
        char* digit_map[10] = {NULL};
        int lens[10];
        bool assigned[10] = {false};

        for (int i = 0; i < 10; ++i) {
            NSString *pattern = [uniquePatterns objectAtIndex:i];
            strcpy(unique_patterns[i], pattern.UTF8String);
            sort_string(unique_patterns[i]);
            lens[i] = strlen(unique_patterns[i]);
        }
        for (int i = 0; i < 4; ++i) {
            NSString *pattern = [outputPatterns objectAtIndex:i];
            strcpy(output_patterns[i], pattern.UTF8String);
            sort_string(output_patterns[i]);
        }

        for(int i = 0; i < 10; ++i) {
            if (lens[i] == 2) { digit_map[1] = unique_patterns[i]; assigned[i] = true; }
            else if (lens[i] == 4) { digit_map[4] = unique_patterns[i]; assigned[i] = true; }
            else if (lens[i] == 3) { digit_map[7] = unique_patterns[i]; assigned[i] = true; }
            else if (lens[i] == 7) { digit_map[8] = unique_patterns[i]; assigned[i] = true; }
        }

        for(int i = 0; i < 10; ++i) {
            if (assigned[i]) continue;
            if (lens[i] == 5 && contains_all_chars_sorted(unique_patterns[i], digit_map[1])) {
                digit_map[3] = unique_patterns[i];
                assigned[i] = true;
            } else if (lens[i] == 6 && contains_all_chars_sorted(unique_patterns[i], digit_map[4])) {
                digit_map[9] = unique_patterns[i];
                assigned[i] = true;
            }
        }

        for(int i = 0; i < 10; ++i) {
            if (assigned[i]) continue;
            if (lens[i] == 6 && contains_all_chars_sorted(unique_patterns[i], digit_map[1])) {
                 digit_map[0] = unique_patterns[i];
                 assigned[i] = true;
                 break;
            }
        }

        for(int i = 0; i < 10; ++i) {
            if (assigned[i]) continue;
            if (lens[i] == 6) {
                 digit_map[6] = unique_patterns[i];
                 assigned[i] = true;
                 break;
            }
        }

        for(int i = 0; i < 10; ++i) {
            if (assigned[i]) continue;
            if (lens[i] == 5) {
                if (contains_all_chars_sorted(digit_map[9], unique_patterns[i])) {
                    digit_map[5] = unique_patterns[i];
                    assigned[i] = true;
                }
            }
        }

        for(int i = 0; i < 10; ++i) {
            if (!assigned[i]) {
                digit_map[2] = unique_patterns[i];
                break;
            }
        }

        int current_number = 0;
        for (int i = 0; i < 4; ++i) {
            for (int j = 0; j < 10; ++j) {
                 if (digit_map[j] != NULL && strcmp(output_patterns[i], digit_map[j]) == 0) {
                    current_number = current_number * 10 + j;
                    break;
                }
            }
        }
        total_sum += current_number;
    }

    printf("%lld\n", total_sum);
    return 0;
}
