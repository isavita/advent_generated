sub MAIN() {
    my ($timestamp-s, $buses-s) = "input.txt".IO.lines;
    my $timestamp = $timestamp-s.Int;
    my @buses = $buses-s.split(',').grep(* ne 'x').map(*.Int);

    my ($min_wait, $selected_bus) = @buses.map({
        ($_ - ($timestamp % $_), $_)
    }).min(*.[0]);

    ($selected_bus * $min_wait).say;
}