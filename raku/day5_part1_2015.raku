
sub MAIN {
    my @strings = 'input.txt'.IO.lines;
    my $nice  = 0;

    for @strings -> $s {
        my $vowels = +$s.comb.grep: * ∈ <a e i o u>;
        my $double = $s ~~ / (.) $0 /;
        my $bad    = $s ~~ / ab | cd | pq | xy /;

        $nice++ if $vowels >= 3 && $double && !$bad;
    }

    say $nice;
}
