
BEGIN {
    RS = ""
    ARGV[1] = "input.txt"
    ARGC = 2
}
{
    total_tiles++
    split($0, lines, "\n")
    split(lines[1], a, " ")
    tile_id[total_tiles] = substr(a[2], 1, length(a[2])-1)
    for (i = 2; i <= 11; i++) T[total_tiles, 0, i-1] = lines[i]
}
function rot(t, src, dst,    i, j, row) {
    for (i = 1; i <= 10; i++) {
        row = ""
        for (j = 10; j >= 1; j--) row = row substr(T[t, src, j], i, 1)
        T[t, dst, i] = row
    }
}
function flip(t, src, dst,    i, j, row) {
    for (i = 1; i <= 10; i++) {
        row = ""
        for (j = 10; j >= 1; j--) row = row substr(T[t, src, i], j, 1)
        T[t, dst, i] = row
    }
}
function get_borders(t, o,    r, s) {
    B[t, o, 0] = T[t, o, 1]
    B[t, o, 2] = T[t, o, 10]
    s = ""; for (r = 1; r <= 10; r++) s = s substr(T[t, o, r], 1, 1); B[t, o, 3] = s
    s = ""; for (r = 1; r <= 10; r++) s = s substr(T[t, o, r], 10, 1); B[t, o, 1] = s
}
function solve(idx,    r, c, i, o) {
    if (idx == total_tiles) return 1
    r = int(idx / SIDE); c = idx % SIDE
    for (i = 1; i <= total_tiles; i++) {
        if (used[i]) continue
        for (o = 0; o < 8; o++) {
            if (r > 0 && B[i, o, 0] != B[placed_tile[idx-SIDE], placed_orient[idx-SIDE], 2]) continue
            if (c > 0 && B[i, o, 3] != B[placed_tile[idx-1], placed_orient[idx-1], 1]) continue
            used[i] = 1; placed_tile[idx] = i; placed_orient[idx] = o
            if (solve(idx + 1)) return 1
            used[i] = 0
        }
    }
    return 0
}
function rot_img(n,    i, j, row, tmp) {
    for (i = 1; i <= n; i++) {
        row = ""; for (j = n; j >= 1; j--) row = row substr(Image[j], i, 1)
        tmp[i] = row
    }
    for (i = 1; i <= n; i++) Image[i] = tmp[i]
}
function flip_img(n,    i, j, row, tmp) {
    for (i = 1; i <= n; i++) {
        row = ""; for (j = n; j >= 1; j--) row = row substr(Image[i], j, 1)
        tmp[i] = row
    }
    for (i = 1; i <= n; i++) Image[i] = tmp[i]
}
function is_monster(r, c) {
    if (substr(Image[r],c+18,1) != "#") return 0
    if (substr(Image[r+1],c,1) != "#" || substr(Image[r+1],c+5,1) != "#" || substr(Image[r+1],c+6,1) != "#") return 0
    if (substr(Image[r+1],c+11,1) != "#" || substr(Image[r+1],c+12,1) != "#" || substr(Image[r+1],c+17,1) != "#") return 0
    if (substr(Image[r+1],c+18,1) != "#" || substr(Image[r+1],c+19,1) != "#") return 0
    if (substr(Image[r+2],c+1,1) != "#" || substr(Image[r+2],c+4,1) != "#" || substr(Image[r+2],c+7,1) != "#") return 0
    if (substr(Image[r+2],c+10,1) != "#" || substr(Image[r+2],c+13,1) != "#" || substr(Image[r+2],c+16,1) != "#") return 0
    return 1
}
function count_monsters(    r, c, cnt) {
    cnt = 0
    for (r = 1; r <= img_size - 2; r++)
        for (c = 1; c <= img_size - 19; c++)
            if (is_monster(r, c)) cnt++
    return cnt
}
END {
    SIDE = int(sqrt(total_tiles) + 0.5)
    for (t = 1; t <= total_tiles; t++) {
        for (o = 1; o <= 3; o++) rot(t, o-1, o)
        flip(t, 0, 4)
        for (o = 5; o <= 7; o++) rot(t, o-1, o)
        for (o = 0; o <= 7; o++) get_borders(t, o)
    }
    solve(0)
    img_row = 1
    for (r = 0; r < SIDE; r++) {
        for (sub_r = 2; sub_r <= 9; sub_r++) {
            row_str = ""
            for (c = 0; c < SIDE; c++)
                row_str = row_str substr(T[placed_tile[r*SIDE+c], placed_orient[r*SIDE+c], sub_r], 2, 8)
            Image[img_row++] = row_str
        }
    }
    img_size = img_row - 1
    for (i = 1; i <= img_size; i++) {
        tmp = Image[i]; total_hashes += gsub(/#/, "#", tmp)
    }
    for (i = 1; i <= 8; i++) {
        m = count_monsters()
        if (m > 0) { print total_hashes - (m * 15); exit }
        rot_img(img_size)
        if (i == 4) flip_img(img_size)
    }
}
