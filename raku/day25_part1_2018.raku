
sub MAIN() {
    my @points = 'input.txt'.IO.lines.map: *.split(',').map(*.Int);
    my $n = @points.elems;
    my @parent = (0 .. $n - 1).Array;
    sub find(int $x) {
        if @parent[$x] != $x {
            @parent[$x] = find(@parent[$x]);
        }
        return @parent[$x];
    }
    sub union(int $x, int $y) {
        my $root-x = find($x);
        my $root-y = find($y);
        if $root-x != $root-y {
            @parent[$root-x] = $root-y;
        }
    }
    sub manhattan-distance(@p1, @p2) {
        return (@p1[0] - @p2[0]).abs + (@p1[1] - @p2[1]).abs + (@p1[2] - @p2[2]).abs + (@p1[3] - @p2[3]).abs;
    }
    for 0 .. $n - 1 -> $i {
        for $i + 1 .. $n - 1 -> $j {
            if manhattan-distance(@points[$i], @points[$j]) <= 3 {
                union($i, $j);
            }
        }
    }
    my $count = 0;
    for 0 .. $n - 1 -> $i {
        if find($i) == $i {
             $count++;
        }
    }
    say $count;
}
