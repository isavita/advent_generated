
sub MAIN() {
    my ($start, $end) = 'input.txt'.IO.lines[0].split('-')».Int;
    my $count = 0;
    for $start..$end -> $num {
        my @d = $num.comb».Int;
        if ([≤] @d) +& (so @d.Bag.values.grep(* ≥ 2)) {
            $count++
        }
    }
    say $count;
}
