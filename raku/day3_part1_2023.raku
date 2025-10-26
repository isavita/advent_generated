
sub MAIN {
    my @m = 'input.txt'.IO.lines>>.comb;
    my $sum = 0;
    my @visited = [False xx @m[0].elems] xx @m.elems;

    for 0..@m.end -> $y {
        for 0..@m[$y].end -> $x {
            next if @visited[$y][$x] || !@m[$y][$x].match(/\d/);
            my ($n, $l) = extract-number(@m[$y], $x);
            $sum += $n if adjacent-to-symbol(@m, $x, $y, $l);
            @visited[$y][$x + $_] = True for ^$l;
        }
    }
    say $sum;
}

sub extract-number(@row, $x is copy) {
    my $s = '';
    while $x < @row.elems && @row[$x].match(/\d/) {
        $s ~= @row[$x++];
    }
    return ($s.Int, $s.chars);
}

sub adjacent-to-symbol(@m, $x is copy, $y, $l) {
    for ^$l {
        return True if check-adjacent(@m, $x + $_, $y);
    }
    False
}

sub check-adjacent(@m, $x, $y) {
    for -1..1 -> $dy {
        for -1..1 -> $dx {
            my ($ax, $ay) = ($x + $dx, $y + $dy);
            next unless 0 <= $ay < @m.elems && 0 <= $ax < @m[$ay].elems;
            return True if @m[$ay][$ax] !~~ /\d|\.|\s/;
        }
    }
    False
}
