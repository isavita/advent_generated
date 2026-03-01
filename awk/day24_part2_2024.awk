
function get_key(o, a, b, t) {
    if (a == "" || b == "") return "INV";
    if (a > b) { t = a; a = b; b = t; }
    return o " " a " " b;
}
function rebuild_rev() {
    for (k in rev) delete rev[k];
    for (g in gates) {
        split(gates[g], p, " ");
        rev[get_key(p[1], p[2], p[3])] = g;
    }
}
function do_swap(a, b, t) {
    swapped[++sw_idx] = a; swapped[++sw_idx] = b;
    t = gates[a]; gates[a] = gates[b]; gates[b] = t;
    rebuild_rev();
}
BEGIN { ARGV[1] = "input.txt"; ARGC = 2; }
$4 == "->" {
    gates[$5] = $2 " " $1 " " $3;
    if ($5 ~ /^z/) {
        n = substr($5, 2) + 0;
        if (n > max_z) max_z = n;
    }
}
END {
    rebuild_rev();
    while (sw_idx < 8) {
        carry = "";
        for (i = 0; i <= max_z; i++) {
            xi = sprintf("x%02d", i); yi = sprintf("y%02d", i); zi = sprintf("z%02d", i);
            if (i == 0) {
                adder = rev[get_key("XOR", xi, yi)];
                carry = rev[get_key("AND", xi, yi)];
            } else {
                bit = rev[get_key("XOR", xi, yi)];
                adder = "";
                if (bit != "") {
                    adder = rev[get_key("XOR", bit, carry)];
                    if (adder != "") {
                        c1 = rev[get_key("AND", xi, yi)];
                        c2 = rev[get_key("AND", bit, carry)];
                        carry = rev[get_key("OR", c1, c2)];
                    }
                }
            }
            if (adder == "") {
                split(gates[zi], g_zi, " ");
                bit = rev[get_key("XOR", xi, yi)];
                if (rev[get_key("XOR", g_zi[2], carry)] != "") { do_swap(bit, g_zi[2]); break; }
                else if (rev[get_key("XOR", g_zi[3], carry)] != "") { do_swap(bit, g_zi[3]); break; }
            } else if (adder != zi) { do_swap(adder, zi); break; }
        }
    }
    for (i=1; i<=8; i++) {
        for (j=i+1; j<=8; j++) {
            if (swapped[i] > swapped[j]) { t = swapped[i]; swapped[i] = swapped[j]; swapped[j] = t; }
        }
    }
    for (i=1; i<=8; i++) printf "%s%s", swapped[i], (i==8 ? ORS : ",");
}
