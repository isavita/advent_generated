BEGIN {
    while (getline < "input.txt") {
        split($0, parts, ")");
        parent = parts[1]; child = parts[2];
        orbits[child] = parent;
    }
    start1 = "YOU"; start2 = "SAN";
    while (start1 in orbits) {
        path1[++depth1] = orbits[start1]; start1 = orbits[start1];
    }
    while (start2 in orbits) {
        path2[++depth2] = orbits[start2]; start2 = orbits[start2];
    }
    while (depth1 > 0 && depth2 > 0 && path1[depth1] == path2[depth2]) {
        depth1--; depth2--;
    }
    print depth1 + depth2;
}