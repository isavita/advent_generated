
use v6;

sub dijkstra(@grid, $end) {
    my ($rows, $cols) = @grid.elems, @grid[0].chars;
    my @pq = (0, |$end);
    my %dist = $end => 0;
    my @dr = -1, 1, 0, 0;
    my @dc = 0, 0, -1, 1;

    while @pq {
        my $d = @pq.shift;
        my ($r, $c) = @pq.shift, @pq.shift;
        next if $d > (%dist{$r}{$c} // Inf);

        for 0..3 -> $i {
            my ($nr, $nc) = $r + @dr[$i], $c + @dc[$i];
            next if $nr < 0 || $nr >= $rows || $nc < 0 || $nc >= $cols;
            next if @grid[$r].substr($c, 1).ord - @grid[$nr].substr($nc, 1).ord > 1;
            my $nd = $d + 1;
            if $nd < (%dist{$nr}{$nc} // Inf) {
                %dist{$nr}{$nc} = $nd;
                @pq.append: ($nd, $nr, $nc);
            }
        }
    }
    return %dist;
}

sub MAIN() {
    my @grid = 'input.txt'.IO.lines;
    my ($start, $end);
    my @a;
    for @grid.kv -> $y, $line {
        for $line.comb.kv -> $x, $c {
            $start = ($y, $x) if $c eq 'S';
            $end   = ($y, $x) if $c eq 'E';
            @a.push: ($y, $x) if $c eq 'a';
        }
    }
    @grid[$start[0]].substr-rw($start[1], 1) = 'a';
    @grid[$end[0]  ].substr-rw($end[1],   1) = 'z';

    my %d = dijkstra(@grid, $end);
    my $best = %d{$start[0]}{$start[1]} // Inf;
    for @a -> ($y, $x) {
        $best = min $best, %d{$y}{$x} // Inf;
    }
    say $best;
}
