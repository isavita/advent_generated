
sub MAIN {
    my @reindeer = 'input.txt'.IO.lines.map: { m:g/(\d+)/.map(*.Int) };

    my $total-time = 2503;

    sub distance ($speed, $fly-time, $rest-time, $time) {
        my $cycle-time = $fly-time + $rest-time;
        my $full-cycles = $time div $cycle-time;
        my $remaining-time = $time % $cycle-time;
        my $dist = $full-cycles * $fly-time * $speed;
        $dist += min($remaining-time, $fly-time) * $speed;
        return $dist;
    }

    my @distances = @reindeer.map: -> @r { distance(@r[0], @r[1], @r[2], $total-time) };
    say @distances.max;

    my @points = (0) xx @reindeer.elems;
    for 1 .. $total-time -> $t {
        my @current-distances = @reindeer.map: -> @r { distance(@r[0], @r[1], @r[2], $t) };
        my $leading-distance = @current-distances.max;
        for ^@reindeer.elems -> $i {
            if @current-distances[$i] == $leading-distance {
                @points[$i]++;
            }
        }
    }
    say @points.max;
}
