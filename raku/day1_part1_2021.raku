
sub MAIN() {
    my $count = 0;
    my @d = "input.txt".IO.lines».Int;
    for 1..^@d -> $i { ++$count if @d[$i] > @d[$i-1] }
    say $count
}
