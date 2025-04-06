
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define MAX_LINE_LEN 1024
#define MAX_MOL_LEN 1024 // Assuming molecule won't exceed this

int main() {
    FILE *fp = fopen("input.txt", "r");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    char line[MAX_LINE_LEN];
    char molecule[MAX_MOL_LEN] = "";

    // Skip rules section, find the molecule
    while (fgets(line, sizeof(line), fp) != NULL) {
        if (line[0] == '\n' || (line[0] == '\r' && line[1] == '\n')) {
            // Found blank line, next line is the molecule
            if (fgets(molecule, sizeof(molecule), fp) != NULL) {
                // Remove trailing newline if present
                molecule[strcspn(molecule, "\n\r")] = 0;
                break;
            } else {
                fprintf(stderr, "Error reading molecule after blank line.\n");
                fclose(fp);
                return 1;
            }
        }
        // Otherwise, just continue reading (skipping rules)
    }
    fclose(fp);

    if (strlen(molecule) == 0) {
         fprintf(stderr, "Could not find molecule string in input.\n");
         return 1;
    }

    int total_elements = 0;
    int rn_count = 0;
    int ar_count = 0;
    int y_count = 0;

    char current_element[3] = {0}; // Max element length seems to be 2 (e.g., Rn) + null terminator

    for (int i = 0; molecule[i] != '\0'; ) {
        total_elements++;
        current_element[0] = molecule[i];
        int element_len = 1;

        if (molecule[i+1] != '\0' && islower(molecule[i+1])) {
            current_element[1] = molecule[i+1];
            current_element[2] = '\0';
            element_len = 2;
        } else {
            current_element[1] = '\0';
        }

        if (strcmp(current_element, "Rn") == 0) {
            rn_count++;
        } else if (strcmp(current_element, "Ar") == 0) {
            ar_count++;
        } else if (strcmp(current_element, "Y") == 0) {
            y_count++;
        }

        i += element_len;
    }

    // Apply the formula derived from analyzing the reduction process structure
    // Steps = TotalElements - Count(Rn) - Count(Ar) - 2 * Count(Y) - 1
    int steps = total_elements - rn_count - ar_count - 2 * y_count - 1;

    printf("%d\n", steps);

    return 0;
}

