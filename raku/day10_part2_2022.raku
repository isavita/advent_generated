
sub MAIN() {
    my @x = 1;
    for 'input.txt'.IO.lines {
        when /noop/ { @x.push: @x[*-1] }
        when /addx\s+(\-*\d+)/ {
            @x.push: @x[*-1];
            @x.push: @x[*-1] + $0.Int;
        }
    }

    my @grid = ['.' xx 40] xx 6;
    my $max = 6*40;
    for ^(@x.elems min $max) -> $i {
        my $crtx = $i % 40;
        my $crty = $i div 40;
        @grid[$crty][$crtx] = '#' if ($crtx - @x[$i]).abs <= 1;
    }

    .join.say for @grid;
}
