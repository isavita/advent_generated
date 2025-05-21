
sub MAIN() {
    my int $size = 10007;
    my @deck = ^$size;

    for 'input.txt'.IO.lines -> $line {
        given $line.trim {
            when 'deal into new stack' {
                @deck = @deck.reverse;
            }
            when /^ 'cut ' (\-?\d+)$/ {
                my $n = +$0;
                @deck = @deck.rotate($n);
            }
            when /^ 'deal with increment ' (\d+)$/ {
                my $n = +$0;
                my @new-deck = (-1) xx $size;
                for 0 ..^ $size -> $i {
                    @new-deck[($i * $n) % $size] = @deck[$i];
                }
                @deck = @new-deck;
            }
        }
    }

    say @deck.first(2019, :k);
}
