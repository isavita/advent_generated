
sub parseMirror(@rows) {
    my @row-bits = 0 xx @rows;
    my @col-bits = 0 xx @rows[0].chars;
    for @rows.kv -> $y, $line {
        for $line.comb.kv -> $x, $c {
            @row-bits[$y] +<= 1;
            @col-bits[$x] +<= 1;
            if $c eq '#' {
                @row-bits[$y]++;
                @col-bits[$x]++;
            }
        }
    }
    return @row-bits, @col-bits;
}

sub mirror-axis(@lines) {
    for 1..^@lines -> $i {
        my Bool $ok = True;
        for 0..^min($i, @lines - $i) -> $j {
            if @lines[$i - 1 - $j] != @lines[$i + $j] {
                $ok = False;
                last;
            }
        }
        return $i if $ok;
    }
    return 0;
}

sub MAIN() {
    my $text = slurp 'input.txt';
    my $sum = 0;
    for $text.split("\n\n") -> $block {
        my @rows = $block.lines;
        my ($rows, $cols) := parseMirror(@rows);
        $sum += mirror-axis($cols);
        $sum += mirror-axis($rows) * 100;
    }
    say $sum;
}
