
sub MAIN {
    my @lines = 'input.txt'.IO.lines;
    my $h   = @lines.elems;
    my $w   = @lines[0].chars;
    my @g   = flat @lines>>.comb>>.Array;

    my %seen;
    my $cycle = 0;
    my $total = 1_000_000_000;

    while $cycle < $total {
        my $key = @g.join;
        if %seen{$key}:exists {
            my $period = $cycle - %seen{$key};
            my $rem    = ($total - $cycle) % $period;
            $cycle = $total - $rem;
            %seen = ();
        }
        %seen{$key} = $cycle;

        # north
        for ^$w -> $x {
            for 1..^$h -> $y {
                next unless @g[$y*$w + $x] eq 'O';
                my $ny = $y;
                $ny-- while $ny > 0 && @g[($ny-1)*$w + $x] eq '.';
                if $ny != $y {
                    @g[$ny*$w + $x] = 'O';
                    @g[$y*$w + $x]  = '.';
                }
            }
        }

        # west
        for ^$h -> $y {
            for 1..^$w -> $x {
                next unless @g[$y*$w + $x] eq 'O';
                my $nx = $x;
                $nx-- while $nx > 0 && @g[$y*$w + ($nx-1)] eq '.';
                if $nx != $x {
                    @g[$y*$w + $nx] = 'O';
                    @g[$y*$w + $x]  = '.';
                }
            }
        }

        # south
        for ^$w -> $x {
            for (0..^$h-1).reverse -> $y {
                next unless @g[$y*$w + $x] eq 'O';
                my $ny = $y;
                $ny++ while $ny < $h-1 && @g[($ny+1)*$w + $x] eq '.';
                if $ny != $y {
                    @g[$ny*$w + $x] = 'O';
                    @g[$y*$w + $x]  = '.';
                }
            }
        }

        # east
        for ^$h -> $y {
            for (0..^$w-1).reverse -> $x {
                next unless @g[$y*$w + $x] eq 'O';
                my $nx = $x;
                $nx++ while $nx < $w-1 && @g[$y*$w + ($nx+1)] eq '.';
                if $nx != $x {
                    @g[$y*$w + $nx] = 'O';
                    @g[$y*$w + $x]  = '.';
                }
            }
        }

        $cycle++;
    }

    my $load = 0;
    for ^$h X ^$w -> ($y,$x) {
        $load += $h - $y if @g[$y*$w + $x] eq 'O';
    }
    put $load;
}
