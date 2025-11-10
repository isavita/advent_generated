
#!/usr/bin/env raku
use v6;

# ------------------------------------------------------------
#  Cheating paths – Advent of Code Day 20 (Part 2) in Raku
# ------------------------------------------------------------
#  Reads the map from "input.txt", finds the number of
#  two‑step (≤20) cheats that save at least 100 picoseconds.
# ------------------------------------------------------------

sub MAIN {
    my $input = "input.txt".IO.slurp;
    my @lines = $input.lines;                     # each line without trailing "\n"
    my $H = +@lines;
    my $W = @lines[0].chars;
    my $N = $H * $W;

    # ---- data containers -------------------------------------------------
    my @walls = Bool::False xx $N;                # true if a wall '#'
    my $start-idx = -1;
    my $end-idx   = -1;

    for 0 .. $H-1 -> $r {
        my $line = @lines[$r];
        for 0 .. $W-1 -> $c {
            my $ch = substr $line, $c, 1;
            my $idx = $r * $W + $c;
            @walls[$idx] = True if $ch eq '#';
            $start-idx = $idx if $ch eq 'S';
            $end-idx   = $idx if $ch eq 'E';
        }
    }

    # ---- movement vectors ------------------------------------------------
    my @dr = (1, -1, 0, 0);
    my @dc = (0, 0, 1, -1);

    # ---- ordinary BFS (walls are obstacles) -----------------------------
    sub bfs_normal($start, $W, $H, @walls) {
        my $N = $H * $W;
        my @dist = (-1) xx $N;
        return @dist if @walls[$start];          # start cannot be a wall
        @dist[$start] = 0;
        my @queue = $start;
        my $head = 0;
        while $head < @queue {
            my $cur = @queue[$head++];
            my $r = $cur div $W;
            my $c = $cur % $W;
            my $d = @dist[$cur];
            for 0 .. 3 -> $i {
                my $nr = $r + @dr[$i];
                my $nc = $c + @dc[$i];
                next if $nr < 0 || $nr >= $H || $nc < 0 || $nc >= $W;
                my $n_idx = $nr * $W + $nc;
                next if @walls[$n_idx];
                next if @dist[$n_idx] != -1;
                @dist[$n_idx] = $d + 1;
                @queue.push($n_idx);
            }
        }
        return @dist;
    }

    # ---- limited BFS (ignores walls, stops after ≤$max_steps) ------------
    # uses a “timestamp” technique so we never have to clear the whole
    # distance array between different start positions.
    sub bfs_limited($start, $W, $H, $max-steps, $cur-version,
                    @dist_c, @dist_ver) {
        # mark the start cell
        @dist_c[$start] = 0;
        @dist_ver[$start] = $cur-version;
        my @queue = $start;          # this list also holds all visited cells
        my $head = 0;
        while $head < @queue {
            my $cur = @queue[$head++];
            my $r = $cur div $W;
            my $c = $cur % $W;
            my $d = @dist_c[$cur];
            next if $d >= $max-steps;               # do not expand beyond the limit
            for 0 .. 3 -> $i {
                my $nr = $r + @dr[$i];
                my $nc = $c + @dc[$i];
                next if $nr < 0 || $nr >= $H || $nc < 0 || $nc >= $W;
                my $n_idx = $nr * $W + $nc;
                next if @dist_ver[$n_idx] == $cur-version;   # already visited in this BFS
                @dist_c[$n_idx] = $d + 1;
                @dist_ver[$n_idx] = $cur-version;
                @queue.push($n_idx);
            }
        }
        return @queue;               # list of all cells reachable within the limit
    }

    # ---- shortest distances from the real start and from the real end ----
    my @dist_s = bfs_normal($start-idx, $W, $H, @walls);
    my @dist_e = bfs_normal($end-idx,   $W, $H, @walls);
    my $normal = @dist_s[$end-idx];
    if $normal == -1 {               # no path at all
        say 0;
        return;
    }

    # ---- helper arrays for the limited BFS ---------------------------------
    my @dist_c = (-1) xx $N;         # distance inside a cheat segment
    my @dist_ver = (0)  xx $N;       # version stamp (timestamp)

    my $MAX_STEPS = 20;
    my $SAVE      = 100;
    my int $cheats = 0;
    my $cur-version = 0;              # will be increased before each start cell

    # ---- main double loop – only over actually reachable cells ------------
    for 0 .. $N-1 -> $start {
        next if @walls[$start];               # cheat can only start on free space
        my $sd = @dist_s[$start];
        next if $sd == -1;                    # must be reachable from the true start

        $cur-version = $cur-version + 1;     # fresh timestamp for this start
        my @visited = bfs_limited($start, $W, $H, $MAX_STEPS,
                                  $cur-version, @dist_c, @dist_ver);

        # examine every cell reachable within the cheat limit
        for @visited -> $end {
            next if $end == $start;           # a cheat must have positive length
            my $s = @dist_c[$end];
            next unless $s > 0 && $s <= $MAX_STEPS;
            next if @walls[$end];
            my $ed = @dist_e[$end];
            next if $ed == -1;                # end must be reachable from the true end

            my $total = $sd + $s + $ed;
            if $total < $normal && ($normal - $total) >= $SAVE {
                $cheats++;
            }
        }
    }

    say $cheats;
}
