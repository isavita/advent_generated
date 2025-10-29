use v6;

sub MAIN() {
    my $serial = "input.txt".IO.slurp.trim.Int;
    my $n = 300;
    my @grid = ([(0) xx $n]) xx $n;
    for 0..^$n -> $y {
        for 0..^$n -> $x {
            my $rack = $x + 11;
            my $p = $rack * ($y + 1) + $serial;
            $p *= $rack;
            $p = ($p div 100) % 10 - 5;
            @grid[$y;$x] = $p;
        }
    }
    my @sum = ([(0) xx ($n+1)]) xx ($n+1);
    for 1..$n -> $y {
        for 1..$n -> $x {
            @sum[$y;$x] = @grid[$y-1;$x-1] + @sum[$y-1;$x] + @sum[$y;$x-1] - @sum[$y-1;$x-1];
        }
    }
    my $max = -Inf;
    my ($mx,$my,$ms);
    for 1..$n -> $size {
        for 1..($n-$size+1) -> $y {
            for 1..($n-$size+1) -> $x {
                my $total = @sum[$y+$size-1;$x+$size-1] - @sum[$y+$size-1;$x-1] - @sum[$y-1;$x+$size-1] + @sum[$y-1;$x-1];
                if $total > $max {
                    $max = $total;
                    $mx = $x;
                    $my = $y;
                    $ms = $size;
                }
            }
        }
    }
    say "$mx,$my,$ms";
}