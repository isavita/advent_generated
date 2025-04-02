
#!/usr/bin/awk -f

BEGIN {
    # Initialize programs array (0-indexed)
    num_progs = 16
    for (i = 0; i < num_progs; i++) {
        progs[i] = sprintf("%c", 97 + i) # 97 is ASCII for 'a'
    }

    # Read the single line of moves from input.txt
    if ((getline line < "input.txt") <= 0) {
        print "Error reading input.txt or file is empty." > "/dev/stderr"
        exit 1
    }
    close("input.txt")

    # Split moves by comma
    n_moves = split(line, moves, ",")

    # Process each move
    for (m = 1; m <= n_moves; m++) {
        move = moves[m]
        type = substr(move, 1, 1)
        params = substr(move, 2)

        if (type == "s") {
            X = int(params)
            # Spin: Rotate the array
            delete temp_progs # Clear temporary array
            # Copy last X elements to start of temp_progs
            for (i = 0; i < X; i++) {
                temp_progs[i] = progs[num_progs - X + i]
            }
            # Copy first (num_progs - X) elements to end of temp_progs
            for (i = 0; i < num_progs - X; i++) {
                 temp_progs[X + i] = progs[i]
            }
             # Copy temp_progs back to progs
            for (i = 0; i < num_progs; i++) {
                progs[i] = temp_progs[i]
            }
        } else if (type == "x") {
            split(params, pos, "/")
            A = int(pos[1])
            B = int(pos[2])
            # Exchange positions A and B
            temp = progs[A]
            progs[A] = progs[B]
            progs[B] = temp
        } else if (type == "p") {
            split(params, names, "/")
            progA = names[1]
            progB = names[2]
            # Partner: Find indices and exchange
            idxA = -1
            idxB = -1
            for (i = 0; i < num_progs; i++) {
                if (progs[i] == progA) idxA = i
                if (progs[i] == progB) idxB = i
                # Optimization: break early if both found (minor for N=16)
                # if (idxA != -1 && idxB != -1) break
            }
            if (idxA != -1 && idxB != -1) {
                temp = progs[idxA]
                progs[idxA] = progs[idxB]
                progs[idxB] = temp
            }
        }
    }

    # Print the final program order
    for (i = 0; i < num_progs; i++) {
        printf "%s", progs[i]
    }
    print "" # Print final newline

    exit # Exit after processing, as input is handled entirely in BEGIN
}

# No main processing block needed as everything is done in BEGIN
