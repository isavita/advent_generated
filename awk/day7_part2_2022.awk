
#!/usr/bin/awk -f

function main() {
    # Initialize current working directory and root directory size
    cwd = "/"
    dirs["/"] = 0

    # Read the input file line by line
    while ((getline line < "input.txt") > 0) {
        # Split line into fields based on space
        split(line, txt, " ")

        if (txt[1] == "$") {
            # Handle commands
            if (txt[2] == "cd") {
                # Change directory
                if (txt[3] == "/") {
                    cwd = "/"
                } else if (txt[3] == "..") {
                    # Go up one level
                    if (cwd != "/") {
                        sub(/\/[^\/]*$/, "", cwd)
                        # Handle case where we go up to root
                        if (cwd == "") {
                            cwd = "/"
                        }
                    }
                } else {
                    # Go down one level
                    # Avoid double slash if cwd is root
                    cwd = (cwd == "/" ? "" : cwd) "/" txt[3]
                    # Ensure the directory entry exists (optional, AWK auto-creates on add)
                    # dirs[cwd] += 0
                }
            }
            # Ignore "ls" command, nothing to do
        } else if (txt[1] != "dir") {
            # Handle file entry: size filename
            size = int(txt[1])
            # Add file size to current directory and all parent directories
            temp_path = cwd
            while (1) {
                dirs[temp_path] += size
                # Stop if we reached the root
                if (temp_path == "/") {
                    break
                }
                # Go to parent directory for the next iteration
                sub(/\/[^\/]*$/, "", temp_path)
                 # Handle case where we go up to root
                if (temp_path == "") {
                    temp_path = "/"
                }
            }
        }
        # Ignore "dir" directory entries, size comes from files within them
    }
    close("input.txt") # Close the file explicitly

    # Calculate needed space and find the smallest directory to delete
    total_space = 70000000
    required_free = 30000000
    used_space = dirs["/"] # Total used space is the size of the root
    available_space = total_space - used_space
    needed_to_free = required_free - available_space

    # Find the smallest directory size that is >= needed_to_free
    min_suitable_size = total_space # Initialize with a value larger than any possible dir size

    for (dir in dirs) {
        dir_size = dirs[dir]
        if (dir_size >= needed_to_free && dir_size < min_suitable_size) {
            min_suitable_size = dir_size
        }
    }

    # Print the final answer
    print min_suitable_size
}

# AWK scripts execute code outside functions directly.
# BEGIN block is typically used for setup. Here we call main() explicitly.
BEGIN {
    main()
}
