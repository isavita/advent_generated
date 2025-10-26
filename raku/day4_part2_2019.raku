
sub MAIN() {
    my ($start, $end) = 'input.txt'.IO.slurp.split('-')».Int;

    my ($p1, $p2) = 0, 0;

    for $start..$end -> $n {
        my @d = $n.comb».Int;

        next unless [<=] @d;                       # never decreases

        my $run = 1;
        my $has-double = False;
        my $has-exact-double = False;

        for 1..^@d {
            if @d[$_] == @d[$_-1] {
                $run++;
            } else {
                $has-double ||= $run ≥ 2;
                $has-exact-double ||= $run == 2;
                $run = 1;
            }
        }
        $has-double ||= $run ≥ 2;
        $has-exact-double ||= $run == 2;

        $p1++ if $has-double;
        $p2++ if $has-exact-double;
    }

    say $p1;
    say $p2;
}
