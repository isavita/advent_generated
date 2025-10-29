
#!/usr/bin/env raku
use v6;

sub MAIN() {
    my $input = do {
        my $fh = open "input.txt", :r;
        my $line = $fh.get;
        $fh.close;
        $line.Int;
    }

    # Part 1 – Manhattan distance in the square‑spiral
    my $layer = 0;
    my $size  = 1;
    while $size * $size < $input {
        $layer++;
        $size += 2;
    }
    my $cycle   = $size - 1;
    my $offset  = $input - ($size - 2) * ($size - 2);
    $offset     = $offset % $cycle;
    my $steps   = $layer + abs $offset - $layer;
    say $steps;

    # Part 2 – First value larger than the input (adjacent‑sum spiral)
    my %grid;
    %grid{"0,0"} = 1;
    my $x = 0;
    my $y = 0;
    my $dx = 0;
    my $dy = -1;
    my $val = 1;

    while $val <= $input {
        if $x == $y
           || ($x < 0 && $x == -$y)
           || ($x > 0 && $x == 1 - $y) {
            my $t = $dx;
            $dx = -$dy;
            $dy = $t;            # turn left
        }
        $x += $dx;
        $y += $dy;

        $val = 0;
        for -1 .. 1 -> $i {
            for -1 .. 1 -> $j {
                $val += %grid{"{$x+$i},{$y+$j}"} // 0;
            }
        }
        %grid{"$x,$y"} = $val;
    }
    say $val;
}
