
sub MAIN {
    my @jumps = 'input.txt'.IO.lines.map: *.Int;
    my $index = 0;
    my $steps = 0;

    while $index >= 0 && $index < @jumps.elems {
        my $offset = @jumps[$index];
        @jumps[$index] += $offset < 3 ?? 1 !! -1;
        $index += $offset;
        $steps++;
    }

    say $steps;
}
