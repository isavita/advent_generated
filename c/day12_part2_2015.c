
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdbool.h>

// --- Configuration ---
#define INPUT_FILENAME "input.txt"
#define MAX_FILE_SIZE 200000 // Adjust if input file is larger

// --- Part 1: Simple Summation ---

// Calculates the sum of all numbers in the input string.
// Ignores structure, simply finds sequences of digits (possibly preceded by '-')
long long calculate_sum_part1(const char *buffer) {
    long long total_sum = 0;
    const char *ptr = buffer;
    char *endptr;

    while (*ptr) {
        // Check for the start of a number (- or digit)
        if (*ptr == '-' || isdigit(*ptr)) {
            // Attempt to parse a number
            long long num = strtoll(ptr, &endptr, 10);
            // Check if strtoll actually consumed characters (i.e., found a valid number)
            if (endptr != ptr) {
                total_sum += num;
                ptr = endptr; // Move pointer past the parsed number
            } else {
                // Not a number (e.g., just a '-' sign alone), advance by one
                ptr++;
            }
        } else {
            // Not the start of a number, advance by one
            ptr++;
        }
    }
    return total_sum;
}

// --- Part 2: Summation with "red" Object Exclusion ---

// Forward declarations for recursive parsing functions
long long parse_value(char **ptr, bool ignore_red);
long long parse_object(char **ptr, bool ignore_red);
long long parse_array(char **ptr, bool ignore_red);

// Helper to skip whitespace characters
void skip_whitespace(char **ptr) {
    while (isspace(**ptr)) {
        (*ptr)++;
    }
}

// Parses a JSON string value and advances the pointer past it.
// Returns true if the parsed string was exactly "red", false otherwise.
// Note: Does not handle complex escape sequences beyond basic \", \\ etc.
// The problem statement implies strings don't contain numbers, simplifying things.
bool parse_string_and_check_red(char **ptr) {
    if (**ptr != '"') return false; // Should start with quote
    (*ptr)++; // Consume opening quote

    char *start = *ptr;
    while (**ptr != '\0' && **ptr != '"') {
         // Handle basic escape sequences if needed, though maybe not necessary for "red" check
         if (**ptr == '\\') {
             (*ptr)++; // Skip the escaped character
             if (**ptr == '\0') return false; // Malformed escape at end of string
         }
         (*ptr)++;
    }
    char *end = *ptr;

    bool is_red = false;
    // Check if the content is exactly "red"
    if ((end - start == 3) && (strncmp(start, "red", 3) == 0)) {
        is_red = true;
    }

    if (**ptr == '"') {
        (*ptr)++; // Consume closing quote
    }
    // else: malformed string (missing closing quote)

    return is_red;
}

// Parses a JSON number and advances the pointer past it.
long long parse_number(char **ptr) {
    char *endptr;
    long long num = strtoll(*ptr, &endptr, 10);
    if (endptr != *ptr) {
        *ptr = endptr; // Advance pointer past the number
        return num;
    }
    // Should not happen if called correctly on a number start, but advance anyway
    if (**ptr != '\0') (*ptr)++;
    return 0; // Return 0 if parsing failed
}


// Parses a JSON array and sums its numeric values according to rules.
long long parse_array(char **ptr, bool ignore_red) {
    long long sum = 0;
    (*ptr)++; // Consume '['
    skip_whitespace(ptr);

    if (**ptr == ']') {
        (*ptr)++; // Consume ']' for empty array
        return 0;
    }

    while (**ptr != '\0') {
        skip_whitespace(ptr);
        // Parse the next value in the array
        sum += parse_value(ptr, ignore_red); // "red" strings in arrays don't cause ignoring
        skip_whitespace(ptr);

        if (**ptr == ']') {
            (*ptr)++; // Consume ']'
            break; // End of array
        } else if (**ptr == ',') {
            (*ptr)++; // Consume ','
        } else {
            // Malformed array (expected ',' or ']')
            // Optionally print error or just break
            fprintf(stderr, "Warning: Malformed array detected near '%.10s'\n", *ptr);
            break;
        }
    }
    return sum;
}

// Parses a JSON object and sums its numeric values according to rules.
// If ignore_red is true and a property has the value "red", returns 0.
long long parse_object(char **ptr, bool ignore_red) {
    long long sum = 0;
    bool contains_red_value = false;
    (*ptr)++; // Consume '{'
    skip_whitespace(ptr);

    if (**ptr == '}') {
        (*ptr)++; // Consume '}' for empty object
        return 0;
    }

    while (**ptr != '\0') {
        skip_whitespace(ptr);

        // 1. Parse Key (must be a string)
        if (**ptr == '"') {
             // We don't need the key value, just advance past it
             parse_string_and_check_red(ptr); // Check if key is "red" (doesn't matter)
        } else {
             fprintf(stderr, "Warning: Malformed object - key not a string near '%.10s'\n", *ptr);
             break; // Error: key must be a string
        }

        skip_whitespace(ptr);

        // 2. Parse Colon
        if (**ptr == ':') {
            (*ptr)++; // Consume ':'
        } else {
             fprintf(stderr, "Warning: Malformed object - missing colon near '%.10s'\n", *ptr);
             break; // Error: expected colon
        }

        skip_whitespace(ptr);

        // 3. Check Value for "red" string *before* recursive parsing
        //    This is crucial for the ignore_red rule.
        if (ignore_red && **ptr == '"') {
            // Peek ahead to see if it's exactly "red"
            char *peek = *ptr + 1; // Point after the opening quote
            if (strncmp(peek, "red\"", 4) == 0) { // Check for 'r', 'e', 'd', '"'
                 contains_red_value = true;
                 // We still need to parse the value to advance the pointer correctly,
                 // even though we will discard the sum later.
            }
        }

        // 4. Parse Value recursively
        long long value_sum = parse_value(ptr, ignore_red);

        // Add the parsed value's sum *only* if the object isn't tainted by red
        if (!contains_red_value) {
            sum += value_sum;
        }

        skip_whitespace(ptr);

        // 5. Check for end of object or next pair
        if (**ptr == '}') {
            (*ptr)++; // Consume '}'
            break; // End of object
        } else if (**ptr == ',') {
            (*ptr)++; // Consume ','
        } else {
            fprintf(stderr, "Warning: Malformed object - expected ',' or '}' near '%.10s'\n", *ptr);
            break; // Error: expected comma or closing brace
        }
    }

    // If the object contained a "red" property value and we're ignoring red, return 0
    if (ignore_red && contains_red_value) {
        return 0;
    } else {
        return sum;
    }
}

// Parses the next JSON value (object, array, string, number, literal)
// and returns its contribution to the sum based on the ignore_red flag.
long long parse_value(char **ptr, bool ignore_red) {
    skip_whitespace(ptr);
    char current_char = **ptr;

    if (current_char == '{') {
        return parse_object(ptr, ignore_red);
    } else if (current_char == '[') {
        return parse_array(ptr, ignore_red);
    } else if (current_char == '"') {
        // Strings contribute 0 to the sum, just advance pointer
        parse_string_and_check_red(ptr);
        return 0;
    } else if (current_char == '-' || isdigit(current_char)) {
        return parse_number(ptr);
    } else if (strncmp(*ptr, "true", 4) == 0) {
        *ptr += 4; return 0; // Literals contribute 0
    } else if (strncmp(*ptr, "false", 5) == 0) {
        *ptr += 5; return 0; // Literals contribute 0
    } else if (strncmp(*ptr, "null", 4) == 0) {
        *ptr += 4; return 0; // Literals contribute 0
    } else {
        // Unknown or end of input - advance cautiously if not null
         if (**ptr != '\0') {
             // Maybe log this? Could indicate malformed JSON or end of structure.
             // fprintf(stderr, "Warning: Skipping unknown character '%c' near '%.10s'\n", **ptr, *ptr);
            (*ptr)++;
         }
        return 0;
    }
}

// Calculates the sum based on Part 2 rules (ignoring "red" objects).
long long calculate_sum_part2(const char *buffer) {
    // Create a mutable pointer for the parsing functions
    char *ptr = (char *)buffer;
    // Start parsing from the beginning, with red object ignoring enabled
    return parse_value(&ptr, true);
}


// --- Main Program ---

int main() {
    FILE *infile = fopen(INPUT_FILENAME, "r");
    if (!infile) {
        perror("Error opening input file");
        fprintf(stderr, "Ensure '%s' exists in the same directory.\n", INPUT_FILENAME);
        return 1;
    }

    // Determine file size
    fseek(infile, 0, SEEK_END);
    long file_size = ftell(infile);
    rewind(infile); // or fseek(infile, 0, SEEK_SET);

    if (file_size < 0 || file_size > MAX_FILE_SIZE) {
         fprintf(stderr, "Error: Invalid or unexpectedly large file size (%ld bytes).\n", file_size);
         fclose(infile);
         return 1;
    }
     if (file_size == 0) {
         fprintf(stderr, "Warning: Input file is empty.\n");
         printf("Part 1 Sum: 0\n");
         printf("Part 2 Sum: 0\n");
         fclose(infile);
         return 0;
     }


    // Allocate memory to read the entire file + null terminator
    char *buffer = malloc(file_size + 1);
    if (!buffer) {
        fprintf(stderr, "Error: Cannot allocate memory (%ld bytes).\n", file_size + 1);
        fclose(infile);
        return 1;
    }

    // Read the file content
    size_t bytes_read = fread(buffer, 1, file_size, infile);
    if (bytes_read != file_size) {
        fprintf(stderr, "Error reading file content. Expected %ld, got %zu bytes.\n", file_size, bytes_read);
        free(buffer);
        fclose(infile);
        return 1;
    }
    buffer[file_size] = '\0'; // Null-terminate the string

    fclose(infile);

    // --- Calculate and Print Results ---
    long long sum1 = calculate_sum_part1(buffer);
    long long sum2 = calculate_sum_part2(buffer);

    printf("Part 1 Sum: %lld\n", sum1);
    printf("Part 2 Sum: %lld\n", sum2);

    // --- Cleanup ---
    free(buffer);

    return 0;
}
