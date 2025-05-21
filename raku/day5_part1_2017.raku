sub MAIN {
    my @jumps = 'input.txt'.IO.lines.map(*.Int).Array;
    my $current_index = 0;
    my $steps = 0;

    while 0 <= $current_index < @jumps.elems {
        my $next_index = $current_index + @jumps[$current_index];
        @jumps[$current_index]++;
        $current_index = $next_index;
        $steps++;
    }

    say $steps;
}