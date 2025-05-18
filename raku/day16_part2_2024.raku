
use v6;
use Algorithm::Heap;

sub main {
    my @grid = 'input.txt'.IO.lines.map: *.chomp;

    my $n = @grid.elems;
    my $m = @grid[0].chars;

    my ($sx, $sy, $ex, $ey);
    for 0 .. $n - 1 -> $r {
        for 0 .. $m - 1 -> $c {
            if @grid[$r].charAt($c) eq 'S' {
                $sx = $r; $sy = $c;
            } elsif @grid[$r].charAt($c) eq 'E' {
                $ex = $r; $ey = $c;
            }
        }
    }

    my @dx = -1, 0, 1, 0; # 0:Up, 1:Right, 2:Down, 3:Left
    my @dy = 0, 1, 0, -1;

    my @dist.dim($n, $m, 4);
    @dist = Inf;
    @dist[$sx][$sy][1] = 0; # Start facing Right

    my $pq = Algorithm::Heap.new: :comparator({ $^a[0] <=> $^b[0] });
    $pq.push: [0, $sx, $sy, 1]; # [cost, r, c, d]

    while $pq.elems {
        my ($cost, $x, $y, $d) = $pq.pop;

        if $cost > @dist[$x][$y][$d] {
            next;
        }

        for (($d + 1) % 4), (($d + 3) % 4) -> $ndir {
            my $nc = $cost + 1000;
            if $nc < @dist[$x][$y][$ndir] {
                @dist[$x][$y][$ndir] = $nc;
                $pq.push: [$nc, $x, $y, $ndir];
            }
        }

        my $nx = $x + @dx[$d];
        my $ny = $y + @dy[$d];

        if 0 <= $nx < $n && 0 <= $ny < $m && @grid[$nx].charAt($ny) ne '#' {
            my $nc = $cost + 1;
            if $nc < @dist[$nx][$ny][$d] {
                @dist[$nx][$ny][$d] = $nc;
                $pq.push: [$nc, $nx, $ny, $d];
            }
        }
    }

    my $best = min(@dist[$ex][$ey]);

    my @used.dim($n, $m);
    @used = False;

    my @vis.dim($n, $m, 4);
    @vis = False;

    my @rev_stack;

    for 0 .. 3 -> $d {
        if @dist[$ex][$ey][$d] == $best {
            @rev_stack.push: [$ex, $ey, $d];
            @vis[$ex][$ey][$d] = True;
        }
    }

    while @rev_stack.elems {
        my ($x, $y, $d) = @rev_stack.pop;

        @used[$x][$y] = True;
        my $costU = @dist[$x][$y][$d];

        for (($d + 1) % 4), (($d + 3) % 4) -> $pd {
            if @dist[$x][$y][$pd] == $costU - 1000 && !@vis[$x][$y][$pd] {
                @vis[$x][$y][$pd] = True;
                @rev_stack.push: [$x, $y, $pd];
            }
        }

        my $px = $x - @dx[$d];
        my $py = $y - @dy[$d];

        if 0 <= $px < $n && 0 <= $py < $m && @grid[$px].charAt($py) ne '#' && @dist[$px][$py][$d] == $costU - 1 && !@vis[$px][$py][$d] {
             @vis[$px][$py][$d] = True;
             @rev_stack.push: [$px, $py, $d];
        }
    }

    say @used.flat.grep(*).elems;
}

main;
