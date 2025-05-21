
sub MAIN() {
    my ($total_paper, $total_ribbon) = 0, 0;

    for "input.txt".IO.lines -> $line {
        my ($l, $w, $h) = $line.split('x').map(*.Int);

        my @sides = $l*$w, $w*$h, $h*$l;
        $total_paper += 2 * [+] @sides + @sides.min;

        my ($d1, $d2, $d3) = ($l, $w, $h).sort;
        $total_ribbon += 2 * ($d1 + $d2) + $l * $w * $h;
    }

    say $total_paper;
    say $total_ribbon;
}
