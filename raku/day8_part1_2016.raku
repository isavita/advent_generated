
#!/usr/bin/env raku
use v6;

constant SCREEN_WIDTH = 50;
constant SCREEN_HEIGHT = 6;

my int @screen[SCREEN_HEIGHT;SCREEN_WIDTH];

sub rect(int $a, int $b) {
    for ^$b -> $y {
        for ^$a -> $x {
            @screen[$y;$x] = 1;
        }
    }
}

sub rotate-row(int $row, int $shift) {
    my int @temp[SCREEN_WIDTH];
    for ^SCREEN_WIDTH -> $i {
        @temp[($i + $shift) % SCREEN_WIDTH] = @screen[$row;$i];
    }
    for ^SCREEN_WIDTH -> $i {
        @screen[$row;$i] = @temp[$i];
    }
}

sub rotate-column(int $col, int $shift) {
    my int @temp[SCREEN_HEIGHT];
    for ^SCREEN_HEIGHT -> $i {
        @temp[($i + $shift) % SCREEN_HEIGHT] = @screen[$i;$col];
    }
    for ^SCREEN_HEIGHT -> $i {
        @screen[$i;$col] = @temp[$i];
    }
}

sub count-lit-pixels() {
    [+] @screen
}

sub MAIN() {
    for 'input.txt'.IO.lines -> $line {
        if $line ~~ /rect \s+ (\d+) 'x' (\d+)/ {
            rect($0.Int, $1.Int);
        } elsif $line ~~ /rotate \s+ row \s+ y\=(\d+) \s+ by \s+ (\d+)/ {
            rotate-row($0.Int, $1.Int);
        } elsif $line ~~ /rotate \s+ column \s+ x\=(\d+) \s+ by \s+ (\d+)/ {
            rotate-column($0.Int, $1.Int);
        }
    }
    say count-lit-pixels();
}
