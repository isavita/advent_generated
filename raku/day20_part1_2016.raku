
sub MAIN {
    my @ranges = 'input.txt'.IO.lines.map: { .split('-')[0,1]».Int };
    @ranges.=sort: { $^a[0] <=> $^b[0] };

    my uint32 $ip = 0;
    for @ranges -> ($s, $e) {
        if $s > $ip { last }
        $ip = $e + 1 if $e >= $ip;
    }
    say $ip;
}
