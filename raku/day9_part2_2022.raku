
sub MAIN {
    my @rope = (0,0) xx 10;
    my %seen;
    for 'input.txt'.IO.lines {
        my ($dir,$n) = .comb(/<upper>|\d+/);
        for ^$n {
            @rope[0] = add(@rope[0], dP($dir));
            for 1..^@rope -> $j {
                @rope[$j] = nextP(@rope[$j-1], @rope[$j]);
            }
            %seen{@rope[*-1].raku} = 1;
        }
    }
    say +%seen;
}

sub add(@a,@b) { @a «+» @b }
sub dP($c) {
    given $c {
        when 'U'|'N'|'^' { (0,1) }
        when 'R'|'E'|'>' { (1,0) }
        when 'D'|'S'|'v' { (0,-1) }
        when 'L'|'W'|'<' { (-1,0) }
    }
}
sub nextP(@h,@t) {
    if (abs(@h[0]-@t[0]) <= 1) && (abs(@h[1]-@t[1]) <= 1) {
        return @t;
    }
    @t «+» (sign(@h[0]-@t[0]), sign(@h[1]-@t[1]))
}
sub sign($n) { $n <=> 0 }
