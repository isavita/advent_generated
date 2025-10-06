#!/usr/bin/awk -f
function hexval(ch) {
    if (ch == "0") return 0;
    if (ch == "1") return 1;
    if (ch == "2") return 2;
    if (ch == "3") return 3;
    if (ch == "4") return 4;
    if (ch == "5") return 5;
    if (ch == "6") return 6;
    if (ch == "7") return 7;
    if (ch == "8") return 8;
    if (ch == "9") return 9;
    if (ch == "A" || ch == "a") return 10;
    if (ch == "B" || ch == "b") return 11;
    if (ch == "C" || ch == "c") return 12;
    if (ch == "D" || ch == "d") return 13;
    if (ch == "E" || ch == "e") return 14;
    if (ch == "F" || ch == "f") return 15;
    return 0;
}
function hex_to_dec(s,   i, v, ch) {
    v = 0;
    for (i = 1; i <= length(s); i++) {
        ch = substr(s, i, 1);
        v = v * 16 + hexval(ch);
    }
    return v;
}
BEGIN {
    x = 0; y = 0;
    vx[0] = 0; vy[0] = 0; n = 1;
    per = 0;
}
END {
    while ((getline line < "input.txt") > 0) {
        p = index(line, "(");
        if (p == 0) continue;
        q = index(line, ")");
        if (q <= p) continue;
        color = substr(line, p + 2, q - (p + 2));
        if (length(color) < 6) continue;
        lenstr = substr(color, 1, 5);
        dir = substr(color, 6, 1);
        len = hex_to_dec(lenstr);
        dx = 0; dy = 0;
        if (dir == "0") dx = len;
        else if (dir == "1") dy = len;
        else if (dir == "2") dx = -len;
        else if (dir == "3") dy = -len;
        x += dx; y += dy;
        vx[n] = x; vy[n] = y; n++;
        per += len;
    }
    area2 = 0;
    for (i = 0; i < n; i++) {
        j = (i + 1) % n;
        area2 += vx[i] * vy[j] - vy[i] * vx[j];
    }
    if (area2 < 0) area2 = -area2;
    area = int(area2 / 2);
    total = area + int(per / 2) + 1;
    print total;
}