
sub MAIN {
    say 'input.txt'.IO.lines.map({
        my ($l, $w, $h) = $_.split('x').map(*.Int);
        my @sides = $l * $w, $w * $h, $h * $l;
        2 * @sides.sum + @sides.min;
    }).sum;
}
