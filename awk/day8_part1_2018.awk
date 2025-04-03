
#!/usr/bin/awk -f

# Global index to track position in the data array d
BEGIN {
    idx = 1
    # Read all numbers from input.txt into array d, separated by whitespace
    while ((getline line < "input.txt") > 0) {
        content = content line " "
    }
    close("input.txt")
    # Split the content into numbers based on whitespace (space, tab, newline)
    n = split(content, d, /[ \t\n]+/)
    # Adjust count if the last element is empty due to trailing whitespace
    if (n > 0 && d[n] == "") {
        n--
    }

    # Call the recursive function starting from index 1 and print the result
    print read_node()
    exit # Exit after processing in BEGIN
}

# Recursive function to process a node and its children
# Local variables: num_child_nodes, num_metadata_entries, total, i (declared by listing after parameters)
function read_node(  num_child_nodes, num_metadata_entries, total, i) {
    # Read header: number of child nodes and number of metadata entries
    num_child_nodes = d[idx++]
    num_metadata_entries = d[idx++]
    total = 0

    # Recursively process child nodes and sum their results
    for (i = 1; i <= num_child_nodes; i++) {
        total += read_node()
    }

    # Sum metadata entries for the current node
    for (i = 1; i <= num_metadata_entries; i++) {
        total += d[idx++]
    }

    return total
}

# No main pattern-action block needed as BEGIN handles the entire process.
# The script reads input.txt, processes the data structure, and prints the sum.
