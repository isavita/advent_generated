
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h> // Use bool for adjacency matrix clarity

// --- Constants ---
// MAX_NODES: Estimated max number of unique computer names.
//            Increased slightly for safety margin based on typical AoC inputs.
#define MAX_NODES 2048
// MAX_NAME_LEN: Max length of a computer name string + 1 for null terminator.
#define MAX_NAME_LEN 16
// FILENAME: Input file name.
#define FILENAME "input.txt"

// --- Global Data Structures ---
// Stores the actual names of the nodes (computers).
char node_names[MAX_NODES][MAX_NAME_LEN];
// Current number of unique nodes discovered.
int node_count = 0;
// Adjacency matrix: adj[i][j] is true if node i and node j are connected.
// Using bool provides clarity over char (0/1). Size is typically the same (1 byte).
bool adj[MAX_NODES][MAX_NODES];

// Stores the node indices of the largest clique found so far.
int max_clique_nodes[MAX_NODES];
// Stores the size of the largest clique found so far.
int max_clique_size = 0;

// --- Helper Function Prototypes ---
/**
 * @brief Gets the unique integer ID for a node name.
 *        If the name is new, assigns a new ID and stores the name.
 * @param name The name of the node (computer).
 * @return The integer ID of the node, or -1 if MAX_NODES is exceeded.
 */
int get_node_id(const char *name);

/**
 * @brief Recursive function to find the maximum clique using backtracking with pruning.
 *
 * @param R Array storing node indices currently in the potential clique being built.
 * @param R_size Current size of the potential clique R.
 * @param P Array storing node indices that are candidates to extend R.
 *          (All nodes in P are connected to all nodes in R).
 * @param P_size Current number of candidates in P.
 */
void find_max_clique_recursive(int R[], int R_size, int P[], int P_size);

/**
 * @brief Comparison function for qsort, used to sort strings alphabetically.
 */
int compare_strings_for_qsort(const void *a, const void *b);

// --- Main Function ---
int main() {
    // Initialize adjacency matrix to all false (no connections initially).
    // memset is efficient for initializing large blocks of memory to zero/false.
    memset(adj, 0, sizeof(adj));

    // --- Read Input and Build Graph ---
    FILE *file = fopen(FILENAME, "r");
    if (!file) {
        perror("Error opening input file");
        fprintf(stderr, "Ensure '%s' exists in the current directory.\n", FILENAME);
        return 1;
    }

    char line[MAX_NAME_LEN * 2 + 2]; // Buffer for "name1-name2\n\0"
    while (fgets(line, sizeof(line), file)) {
        // Find the hyphen separating the names.
        char *hyphen = strchr(line, '-');
        if (!hyphen) {
             // Ignore blank lines silently, warn about other malformed lines.
            if (strspn(line, " \t\r\n") != strlen(line)) {
                 fprintf(stderr, "Warning: Malformed line (no hyphen): %s", line);
            }
            continue;
        }

        // Split the line into two names.
        *hyphen = '\0'; // Null-terminate the first name.
        char *name1 = line;
        char *name2 = hyphen + 1;

        // Remove trailing newline/carriage return from the second name.
        name2[strcspn(name2, "\r\n")] = '\0';

        // Skip if names are empty after parsing (should not happen with valid input)
        if (strlen(name1) == 0 || strlen(name2) == 0) {
            continue;
        }

        // Get unique integer IDs for each name, adding them if new.
        int id1 = get_node_id(name1);
        int id2 = get_node_id(name2);

        // If IDs are valid (within bounds and different), mark the connection.
        if (id1 >= 0 && id2 >= 0) {
            if (id1 != id2) { // Ensure no self-loops are added
                adj[id1][id2] = true;
                adj[id2][id1] = true; // Undirected graph
            }
        } else {
             // get_node_id prints error if MAX_NODES is exceeded
             fclose(file);
             return 1;
        }
    }
    fclose(file);

    // --- Find the Maximum Clique ---
    if (node_count == 0) {
        printf("No nodes found in input.\n");
        return 0;
    }

    // Initial candidates (P) are all nodes (0 to node_count - 1).
    // Using stack allocation for initial_P and initial_R. Ensure MAX_NODES
    // is large enough or consider dynamic allocation if node_count can be huge.
    // Variable Length Arrays (VLAs) like int initial_P[node_count] are C99+.
    // Using a fixed MAX_NODES buffer is more portable.
    int initial_P[MAX_NODES];
    for (int i = 0; i < node_count; ++i) {
        initial_P[i] = i;
    }
    int initial_R[MAX_NODES]; // Empty initial clique

    // Start the recursive search.
    find_max_clique_recursive(initial_R, 0, initial_P, node_count);

    // --- Prepare and Print Output ---
    if (max_clique_size == 0) {
        printf("No clique found (graph might be empty or have no edges).\n");
        return 0;
    }

    // Create an array of pointers to the names in the maximum clique.
    // Using VLA here for convenience, assuming max_clique_size is small.
    char *result_names[max_clique_size];
    for (int i = 0; i < max_clique_size; ++i) {
        // Basic bounds check for safety before accessing node_names
        if (max_clique_nodes[i] >= 0 && max_clique_nodes[i] < node_count) {
             result_names[i] = node_names[max_clique_nodes[i]];
        } else {
             fprintf(stderr, "Error: Invalid node index %d in max clique array.\n", max_clique_nodes[i]);
             return 1; // Internal error
        }
    }

    // Sort the names alphabetically using qsort.
    qsort(result_names, max_clique_size, sizeof(char *), compare_strings_for_qsort);

    // Print the sorted names, joined by commas, to standard output.
    for (int i = 0; i < max_clique_size; ++i) {
        // fwrite can be slightly more efficient than printf for raw strings.
        fwrite(result_names[i], 1, strlen(result_names[i]), stdout);
        if (i < max_clique_size - 1) {
            putchar(','); // Print separator
        }
    }
    putchar('\n'); // Final newline

    return 0;
}

// --- Helper Function Implementations ---

int get_node_id(const char *name) {
    // Linear search for existing node name.
    // For the scale of typical AoC problems (~1k-2k nodes), this is often acceptable.
    // A hash map could improve this from O(N) to O(1) average per lookup.
    for (int i = 0; i < node_count; ++i) {
        // Use strcmp for direct comparison (names are null-terminated).
        if (strcmp(node_names[i], name) == 0) {
            return i; // Found
        }
    }

    // Not found, add new node if space permits.
    if (node_count >= MAX_NODES) {
        fprintf(stderr, "Error: Maximum node limit (%d) exceeded trying to add '%s'.\n", MAX_NODES, name);
        return -1; // Error code
    }

    // Copy the new name, ensuring null termination.
    strncpy(node_names[node_count], name, MAX_NAME_LEN - 1);
    node_names[node_count][MAX_NAME_LEN - 1] = '\0';

    // Return the new ID and increment count for the next node.
    return node_count++;
}


void find_max_clique_recursive(int R[], int R_size, int P[], int P_size) {
    // Stack-allocated buffers for the next recursive call's parameters.
    // These need to be large enough to hold the maximum possible number of nodes.
    // Risk of stack overflow if MAX_NODES is very large and recursion is deep.
    int next_R[MAX_NODES];
    int next_P[MAX_NODES];

    // Base Case 1: No candidates left to extend the current clique R.
    // R is now a *maximal* clique (can't be extended with P).
    if (P_size == 0) {
        // Check if this maximal clique is larger than the current maximum found.
        if (R_size > max_clique_size) {
            max_clique_size = R_size;
            // Store this new best clique.
            memcpy(max_clique_nodes, R, R_size * sizeof(int));
        }
        return; // Backtrack
    }

    // Base Case 2: Pruning optimization.
    // If the current clique size plus all remaining candidates cannot possibly
    // form a clique larger than the best one already found, abandon this path.
    if (R_size + P_size <= max_clique_size) {
        return; // Prune
    }

    // --- Recursive Step ---
    // Iterate through candidate nodes 'v' in P.
    // We need a copy of P because we effectively modify the candidate set for recursive calls.
    // Using VLA for copy; relies on C99+. Fixed buffer P_copy[MAX_NODES] is alternative.
    int P_copy[P_size];
    memcpy(P_copy, P, P_size * sizeof(int));

    for (int i = 0; i < P_size; ++i) {
        int v = P_copy[i]; // Select the i-th candidate 'v'.

        // --- Explore cliques containing 'v' ---

        // 1. Form the new potential clique R' = R union {v}.
        memcpy(next_R, R, R_size * sizeof(int));
        next_R[R_size] = v;
        int next_R_size = R_size + 1;

        // 2. Form the new candidate set P' for the recursive call.
        //    P' consists of nodes in P (specifically P_copy[j] where j > i)
        //    that are connected to 'v'. They are already guaranteed to be
        //    connected to all nodes in R because they were in the original P.
        int next_P_size = 0;
        for (int j = i + 1; j < P_size; ++j) {
            int w = P_copy[j]; // Candidate node after v in the P_copy list.
            if (adj[v][w]) { // Fast O(1) check using the adjacency matrix.
                next_P[next_P_size++] = w;
            }
        }

        // 3. Make the recursive call to explore cliques extending R'.
        find_max_clique_recursive(next_R, next_R_size, next_P, next_P_size);

        // Backtracking is handled implicitly by the loop structure.
        // After the recursive call returns, the loop continues to the next 'i',
        // effectively considering possibilities where P_copy[i] ('v') is *not* included,
        // but P_copy[i+1] might be.
    }
}


int compare_strings_for_qsort(const void *a, const void *b) {
    // Cast void pointers to the actual type (pointer to char*) and dereference.
    const char *str_a = *(const char **)a;
    const char *str_b = *(const char **)b;
    // Use strcmp for standard lexicographical comparison.
    return strcmp(str_a, str_b);
}
