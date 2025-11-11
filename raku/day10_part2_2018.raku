
#!/usr/bin/env raku

class Star {
    has Int $.x;
    has Int $.y;
    has Int $.vx;
    has Int $.vy;
}

sub MAIN() {
    my @stars = 'input.txt'.IO.lines.map({
        my ($x, $y, $vx, $vy) = .comb(/\-?\d+/).map(*.Int);
        Star.new(:$x, :$y, :$vx, :$vy)
    });

    my $best-t = 0;
    my $min-area = Inf;

    for 1..100000 -> $t {
        my @xs = @stars.map: { .x + .vx * $t };
        my @ys = @stars.map: { .y + .vy * $t };
        
        my $area = (@xs.max - @xs.min) * (@ys.max - @ys.min);
        
        if $area < $min-area {
            $min-area = $area;
            $best-t = $t;
        }
    }

    say $best-t;
}
