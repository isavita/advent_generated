
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

typedef struct {
    int id;
    int start;
    int end;
} FileSegment;

int main() {
    FILE *fp = fopen("input.txt", "r");
    if (fp == NULL) {
        return 1;
    }
    char input_line[100000];
    fgets(input_line, sizeof(input_line), fp);
    fclose(fp);

    input_line[strcspn(input_line, "\n")] = 0;

    int total_size = 0;
    int input_len = strlen(input_line);
    for (int i = 0; i < input_len; i++) {
        if (isdigit(input_line[i])) {
            total_size += input_line[i] - '0';
        }
    }

    if (total_size == 0) {
        printf("0\n");
        return 0;
    }

    int num_files = (input_len + 1) / 2;
    int *disk = (int *)malloc(total_size * sizeof(int));
    FileSegment *files = (FileSegment *)malloc(num_files * sizeof(FileSegment));

    int current_pos = 0;
    int file_id_counter = 0;
    for (int i = 0; i < input_len; i++) {
        int length = input_line[i] - '0';
        if (i % 2 == 0) {
            files[file_id_counter].id = file_id_counter;
            files[file_id_counter].start = current_pos;
            files[file_id_counter].end = current_pos + length - 1;
            for (int j = 0; j < length; j++) {
                disk[current_pos + j] = file_id_counter;
            }
            file_id_counter++;
        } else {
            for (int j = 0; j < length; j++) {
                disk[current_pos + j] = -1;
            }
        }
        current_pos += length;
    }

    for (int i = num_files - 1; i >= 0; i--) {
        FileSegment file = files[i];
        int file_len = file.end - file.start + 1;
        
        int best_span_start = -1;
        int current_span_len = 0;

        for (int j = 0; j < file.start; j++) {
            if (disk[j] == -1) {
                if (current_span_len == 0) {
                    best_span_start = j;
                }
                current_span_len++;
                if (current_span_len == file_len) {
                    break;
                }
            } else {
                current_span_len = 0;
                best_span_start = -1;
            }
        }

        if (best_span_start != -1 && current_span_len == file_len) {
            for (int j = 0; j < file_len; j++) {
                disk[best_span_start + j] = file.id;
            }
            for (int j = file.start; j <= file.end; j++) {
                disk[j] = -1;
            }
        }
    }

    long long checksum = 0;
    for (int i = 0; i < total_size; i++) {
        if (disk[i] != -1) {
            checksum += (long long)i * disk[i];
        }
    }

    printf("%lld\n", checksum);

    free(disk);
    free(files);

    return 0;
}
