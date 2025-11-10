
#!/usr/bin/env raku
#| Reads the puzzle data from `input.txt` and prints the sum of the
#| positions of the live plants after 20 generations.

sub MAIN {
    my $file = 'input.txt';
    my $content = $file.IO.slurp // die "Cannot read $file";
    my @lines = $content.lines;

    my $initial;                     # the starting line, e.g. "#..#.#..##......###...###"
    my %rules;                       # maps a 5‑character pattern to its result ('.' or '#')

    for @lines -> $line {
        if $line ~~ / ^ 'initial state:' \s+ (.*) / {
            $initial = $0.Str.trim;
        }
        elsif $line ~~ / ^ (.*) '=>' \s+ (.) / {
            my $pattern = $0.Str.trim;
            my $result  = $1.Str.trim;
            %rules{$pattern} = $result;
        }
    }

    constant $GENERATIONS = 20;      # as required by the problem statement

    # --------------------------------------------------------------------
    # State representation: a SetHash stores *only* the positions that are '#'.
    # This gives O(1) membership tests and eliminates the need for sorting.
    # --------------------------------------------------------------------
    my SetHash $live .= new;
    for $initial.comb.kv -> $pos, $c {
        $live{$pos} = True if $c eq '#';
    }

    # Keep the current bounds of the occupied cells – they shrink the search
    # space for the next generation.
    my $min = $live ?? $live.keys.min !! 0;
    my $max = $live ?? $live.keys.max !! 0;

    for ^$GENERATIONS -> $gen {
        my SetHash $new-live .= new;
        my $start = $min - 2;               # we may need to start a little left
        my $end   = $max + 2;               # …and a little right

        for $start .. $end -> $i {
            # Build the 5‑character window centred on $i.
            my @pattern = do for -2..2 -> $off {
                $live{$i + $off} ?? '#' !! '.';
            };
            my $pattern-str = @pattern.join;

            # If a rule exists and tells us to plant at $i, keep it.
            if %rules{$pattern-str} and %rules{$pattern-str} eq '#' {
                $new-live{$i} = True;
            }
        }

        # Replace the old state with the newly computed one.
        $live = $new-live;

        # Update the bounds for the next generation.
        if $live {
            $min = $live.keys.min;
            $max = $live.keys.max;
        } else {
            $min = $max = 0;                 # no plants – bounds are irrelevant
        }
    }

    my $sum = $live.keys.sum;   # the answer required by the puzzle
    say $sum;
}
