
use v6;

sub MAIN {
    my @discs = parse-input('input.txt');
    my $t = 0;
    loop {
        my $valid = True;
        for @discs.kv -> $i, ($M, $K) {
            if (($K + $t + $i + 1) % $M) != 0 {
                $valid = False;
                last;
            }
        }
        if $valid {
            say $t;
            last;
        }
        $t++;
    }
}

sub parse-input(Str $filename) {
    my @discs;
    for $filename.IO.lines {
        when / 'Disc #' \d+ ' has ' (\d+) ' positions; at time=0, it is at position ' (\d+) '.' / {
            @discs.push: [ $0, $1 ];
        }
    }
    return @discs;
}
