BEGIN {
    FS = "[~,]";
    while (getline < "input.txt") {
        if (NF == 0) continue;
        bricks[++n] = $0;
    }
    for (i = 1; i <= n; i++) {
        split(bricks[i], coords, "[~,]");
        mini_x[i] = coords[1]; mini_y[i] = coords[2]; mini_z[i] = coords[3];
        maxi_x[i] = coords[4]; maxi_y[i] = coords[5]; maxi_z[i] = coords[6];
        basedOn[i] = ""; support[i] = "";
    }
    for (i = 1; i <= n; i++) {
        for (j = i + 1; j <= n; j++) {
            if (maxi_z[i] > maxi_z[j]) {
                swap = bricks[i]; bricks[i] = bricks[j]; bricks[j] = swap;
                swap = mini_x[i]; mini_x[i] = mini_x[j]; mini_x[j] = swap;
                swap = mini_y[i]; mini_y[i] = mini_y[j]; mini_y[j] = swap;
                swap = mini_z[i]; mini_z[i] = mini_z[j]; mini_z[j] = swap;
                swap = maxi_x[i]; maxi_x[i] = maxi_x[j]; maxi_x[j] = swap;
                swap = maxi_y[i]; maxi_y[i] = maxi_y[j]; maxi_y[j] = swap;
                swap = maxi_z[i]; maxi_z[i] = maxi_z[j]; maxi_z[j] = swap;
            }
        }
    }
    for (i = 1; i <= n; i++) {
        supportZ = 0;
        basedBricks = "";
        for (j = i - 1; j >= 1; j--) {
            ix = (max(mini_x[i], mini_x[j]) <= min(maxi_x[i], maxi_x[j]));
            iy = (max(mini_y[i], mini_y[j]) <= min(maxi_y[i], maxi_y[j]));
            if (ix && iy) {
                if (maxi_z[j] == supportZ) {
                    basedBricks = basedBricks "," j;
                } else if (maxi_z[j] > supportZ) {
                    supportZ = maxi_z[j];
                    basedBricks = j;
                }
            }
        }
        basedOn[i] = basedBricks;
        split(basedBricks, arr, ",");
        for (k in arr) {
            if (arr[k] == "") continue;
            support[arr[k]] = support[arr[k]] "," i;
        }
        deltaZ = maxi_z[i] - mini_z[i];
        mini_z[i] = supportZ + 1;
        maxi_z[i] = mini_z[i] + deltaZ;
    }
    cnt = 0;
    for (i = 1; i <= n; i++) {
        delete falling;
        split(support[i], sarr, ",");
        for (j in sarr) {
            if (sarr[j] == "") continue;
            split(basedOn[sarr[j]], barr, ",");
            if (length(barr) == 1 && barr[1] == i) {
                q[1] = sarr[j]; qn = 1;
                falling[sarr[j]] = 1;
                while (qn > 0) {
                    cur = q[qn]; qn--;
                    split(support[cur], carr, ",");
                    for (k in carr) {
                        if (carr[k] == "") continue;
                        split(basedOn[carr[k]], darr, ",");
                        canFall = 1;
                        for (l in darr) {
                            if (darr[l] == "") continue;
                            if (!(darr[l] in falling) && darr[l] != i) {
                                canFall = 0;
                                break;
                            }
                        }
                        if (canFall && !(carr[k] in falling)) {
                            falling[carr[k]] = 1;
                            q[++qn] = carr[k];
                        }
                    }
                }
            }
        }
        cnt += length(falling);
    }
    print cnt;
}

function max(a, b) { return a > b ? a : b }
function min(a, b) { return a < b ? a : b }