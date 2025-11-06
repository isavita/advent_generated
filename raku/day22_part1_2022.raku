
use v6.d;

sub MAIN {
    my $input = slurp 'input.txt';
    my ($map-str, $path) = $input.split("\n\n");

    my @lines = $map-str.lines;
    my $rows = @lines.elems;
    my $cols = @lines>>.chars.max;

    my %walls;
    my %open;
    my $start;

    for @lines.kv -> $r, $line {
        for $line.comb.kv -> $c, $char {
            given $char {
                when '#' { %walls{"$r,$c"} = True }
                when '.' {
                    %open{"$r,$c"} = True;
                    $start //= ($r, $c);
                }
            }
        }
    }

    my regex num { \d+ };
    my @tokens = $path.comb(/<num> | <[RL]>/);

    my ($r, $c) = $start;
    my $dir = 0;          # 0=East, 1=South, 2=West, 3=North
    my @dr = 0, 1, 0, -1;
    my @dc = 1, 0, -1, 0;

    for @tokens {
        when / <num> / {
            my $steps = +$_;
            for ^$steps {
                my $nr = ($r + @dr[$dir]) % $rows;
                my $nc = ($c + @dc[$dir]) % $cols;

                while !%open{"$nr,$nc"} && !%walls{"$nr,$nc"} {
                    $nr = ($nr + @dr[$dir]) % $rows;
                    $nc = ($nc + @dc[$dir]) % $cols;
                }

                if %walls{"$nr,$nc"} {
                    last;
                } else {
                    ($r, $c) = ($nr, $nc);
                }
            }
        }
        when 'R' { $dir = ($dir + 1) % 4 }
        when 'L' { $dir = ($dir + 3) % 4 }
    }

    say 1000 * ($r + 1) + 4 * ($c + 1) + $dir;
}
