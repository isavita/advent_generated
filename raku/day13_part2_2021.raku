
sub MAIN() {
    my @points;
    my @folds;
    my $points = True;

    for 'input.txt'.IO.lines {
        when '' { $points = False; next }
        if $points {
            @points.push: .split(',').map: *.Int
        } else {
            my ($axis, $val) = .split('=');
            @folds.push: [$axis.contains('x') ?? $val.Int !! 0,
                          $axis.contains('y') ?? $val.Int !! 0]
        }
    }

    for @folds.kv -> $i, @f {
        my %new;
        for @points {
            my ($x, $y) = @$_;
            if @f[0] && $x > @f[0] { $x = @f[0] - ($x - @f[0]) }
            if @f[1] && $y > @f[1] { $y = @f[1] - ($y - @f[1]) }
            %new{"$x,$y"} = 1
        }
        @points = %new.keys.map: *.split(',').map: *.Int;
        say "Number of dots visible after first fold: {@points.elems}" if $i == 0
    }

    my ($maxX, $maxY) = 0, 0;
    for @points -> [$x, $y] {
        $maxX = $x if $x > $maxX;
        $maxY = $y if $y > $maxY
    }

    my @grid = [ (' ' xx $maxX + 1).Array xx $maxY + 1 ];
    for @points -> [$x, $y] { @grid[$y][$x] = '#' }

    .join.say for @grid
}
