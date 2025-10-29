
sub MAIN() {
    my $total = 0;
    for "input.txt".IO.lines -> $line {
        my $special = $line.comb.grep({ $_ eq "\\" || $_ eq '"' }).elems;
        $total += $special + 2;      # +2 for the surrounding quotes
    }
    say $total;
}
