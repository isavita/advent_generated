
use v6;

sub parse-claim(Str $line) {
    $line ~~ / '#' (\d+) \s* '@' \s* (\d+) ',' (\d+) ':' \s* (\d+) 'x' (\d+) /;
    { id => +$0, left => +$1, top => +$2, width => +$3, height => +$4 }
}

sub MAIN {
    my @claims = 'input.txt'.IO.lines.map: &parse-claim;

    my @fabric; @fabric[999;999] = 0;           # pre-size 1000×1000
    my %claim  = @claims.map: { .<id> => True };

    for @claims -> %c {
        for %c<left> .. %c<left> + %c<width> - 1 -> $x {
            for %c<top>  .. %c<top>  + %c<height> - 1 -> $y {
                my $prev = @fabric[$y;$x];
                if $prev {
                    %claim{$prev} = False;
                    %claim{%c<id>} = False;
                }
                @fabric[$y;$x] = %c<id>;
            }
        }
    }

    say @fabric.grep(*.defined).elems;         # overlapping area
    say %claim.keys.grep({ %claim{$_} })[0];  # non-overlapping ID
}
