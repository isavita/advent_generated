
BEGIN {
    FS=": ";
    while (getline < "input.txt") {
        depth[$1] = $2;
    }
    for (delay = 0; ; delay++) {
        caught = 0;
        for (d in depth) {
            range = depth[d];
            if ((d + delay) % (2 * (range - 1)) == 0) {
                caught = 1;
                break;
            }
        }
        if (!caught) {
            print delay;
            exit;
        }
    }
}
