
sub MAIN() {
    my %cube;
    for "input.txt".IO.lines -> $l {
        my ($x,$y,$z) = $l.split(',').map(*.Int);
        # encode coordinate as a single integer (0 ≤ x,y,z < 100)
        %cube{ $x * 10_000 + $y * 100 + $z } = True;
    }

    my $area = 0;
    for %cube.keys -> $key {
        $area += 6;
        $area-- if %cube{ $key - 10_000 } :exists;   # x‑1
        $area-- if %cube{ $key + 10_000 } :exists;   # x+1
        $area-- if %cube{ $key -   100 } :exists;   # y‑1
        $area-- if %cube{ $key +   100 } :exists;   # y+1
        $area-- if %cube{ $key -     1 } :exists;   # z‑1
        $area-- if %cube{ $key +     1 } :exists;   # z+1
    }

    say $area;
}
