
use v6;

sub MAIN {
    my $grid-serial = "input.txt".IO.slurp.trim.Int;

    my @grid[301;301];

    for 1..300 -> $i {
        for 1..300 -> $j {
            my $rack = $i + 10;
            my $power = (($rack * $j + $grid-serial) * $rack / 100 % 10) - 5;
            @grid[$i;$j] = $power + @grid[$i-1;$j] + @grid[$i;$j-1] - @grid[$i-1;$j-1];
        }
    }

    my ($max-power, $max-x, $max-y) = -Inf;

    for 1..298 -> $i {
        for 1..298 -> $j {
            my $power = @grid[$i+2;$j+2] - @grid[$i-1;$j+2] - @grid[$i+2;$j-1] + @grid[$i-1;$j-1];
            if $power > $max-power {
                ($max-power, $max-x, $max-y) = $power, $i, $j;
            }
        }
    }

    put "$max-x,$max-y";
}
