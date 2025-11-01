
sub manhattan(\p, \q) { abs(p[0] - q[0]) + abs(p[1] - q[1]) }

sub impossible(@sensors, Int $y) {
    my %pts;
    for @sensors -> \s {
        my Int $d = s[2] - abs(s[0][1] - $y);
        next if $d < 0;
        %pts{s[0][0] + $_} = True for -$d..$d;
    }
    for @sensors -> \s {
        %pts{s[1][0]}:delete if s[1][1] == $y;
    }
    +%pts
}

sub MAIN {
    my @sensors;
    for 'input.txt'.IO.lines {
        my ($sx,$sy,$bx,$by) = .comb(/ '-'? \d+ /).map: *.Int;
        @sensors.push: [[$sx,$sy], [$bx,$by], manhattan([$sx,$sy],[$bx,$by])];
    }
    say impossible(@sensors, 2000000)
}
