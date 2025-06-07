
#include <stdio.h>   // For file operations (fopen, fgetc, printf) and perror
#include <stdlib.h>  // For memory allocation (malloc, free)
#include <ctype.h>   // For character type checking (isdigit)

// Define a constant for the input filename for easy modification.
#define INPUT_FILENAME "input.txt"
// Use a named constant to represent free space for better code readability.
#define FREE_SPACE -1

/**
 * @brief Main entry point of the program.
 *
 * This program solves the "Disk Fragmenter" challenge by reading a compact
 * disk map from `input.txt`, simulating a disk compaction process, and
 * calculating a final checksum based on the compacted state.
 *
 * The solution is optimized for performance and memory usage:
 * 1. A first pass over the input file calculates the exact total disk size,
 *    allowing for a single, precise memory allocation for the disk array.
 *    This avoids the overhead of dynamic resizing (e.g., realloc).
 * 2. The disk is represented as an integer array, where each element holds
 *    a file ID or a special value for free space.
 * 3. The compaction is performed efficiently in O(N) time using a two-pointer
 *    algorithm, where N is the total number of blocks on the disk. One pointer
 *    finds free space from the left, and the other finds file blocks from the
 *    right, swapping them until the disk is contiguous.
 * 4. The final checksum is calculated, using an `unsigned long long` to prevent
 *    overflow with large disks.
 */
int main(void) {
    FILE *fp;
    int c;
    long total_size = 0;

    // --- Pass 1: Calculate total disk size to pre-allocate memory ---
    fp = fopen(INPUT_FILENAME, "r");
    if (fp == NULL) {
        perror("Error opening input file");
        return 1;
    }

    while ((c = fgetc(fp)) != EOF && c != '\n') {
        if (isdigit(c)) {
            // The total number of blocks is the sum of all digits in the map.
            total_size += c - '0';
        }
    }
    
    // Edge case: If the input is empty or contains no digits, the disk is empty.
    if (total_size == 0) {
        printf("0\n"); // The checksum of an empty disk is 0.
        fclose(fp);
        return 0;
    }

    // --- Allocate Memory for the Disk Map ---
    // Using `long` for size and indices to handle potentially very large disks.
    int *disk = malloc(total_size * sizeof(int));
    if (disk == NULL) {
        fprintf(stderr, "Error: Failed to allocate memory for disk map of size %ld.\n", total_size);
        fclose(fp);
        return 1;
    }

    // --- Pass 2: Populate the disk map with initial file and free space data ---
    rewind(fp);
    long current_pos = 0;
    int digit_index = 0;

    while ((c = fgetc(fp)) != EOF && c != '\n') {
        if (isdigit(c)) {
            int length = c - '0';
            
            // Even-indexed digits (0, 2, 4...) represent file lengths.
            // Odd-indexed digits (1, 3, 5...) represent free space lengths.
            if (digit_index % 2 == 0) {
                int file_id = digit_index / 2;
                for (int i = 0; i < length; i++) {
                    disk[current_pos++] = file_id;
                }
            } else { // This is a free space segment
                for (int i = 0; i < length; i++) {
                    disk[current_pos++] = FREE_SPACE;
                }
            }
            digit_index++;
        }
    }
    fclose(fp);

    // --- Compaction Phase ---
    // Use an efficient two-pointer algorithm to compact the disk in-place.
    // 'left' seeks the next free space from the start.
    // 'right' seeks the last file block from the end.
    long left = 0;
    long right = total_size - 1;

    while (left < right) {
        // Find the first free spot from the left.
        while (left < right && disk[left] != FREE_SPACE) {
            left++;
        }
        // Find the last file block from the right.
        while (left < right && disk[right] == FREE_SPACE) {
            right--;
        }

        // If the pointers haven't crossed, swap the file block into the free space.
        if (left < right) {
            disk[left] = disk[right];
            disk[right] = FREE_SPACE;
            // Advance pointers to continue the search.
            left++;
            right--;
        }
    }

    // --- Checksum Calculation ---
    // The checksum can become very large, so use `unsigned long long`.
    unsigned long long checksum = 0;
    for (long i = 0; i < total_size; i++) {
        // Only blocks containing files contribute to the checksum.
        if (disk[i] != FREE_SPACE) {
            checksum += (unsigned long long)i * disk[i];
        }
    }

    // --- Output and Cleanup ---
    printf("%llu\n", checksum);
    free(disk);

    return 0;
}
