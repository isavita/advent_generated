
sub MAIN {
    my $steps = 'input.txt'.IO.slurp.Int;

    my @buffer = 0;
    my $current-pos = 0;

    for 1 .. 2017 -> $i {
        $current-pos = ($current-pos + $steps) % @buffer.elems + 1;
        @buffer.splice($current-pos, 0, $i);
    }

    say @buffer[($current-pos + 1) % @buffer.elems];
}
