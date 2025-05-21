
sub MAIN {
    my $first-row = 'input.txt'.IO.lines.first;
    my $total-rows = 400000;

    my $current-row = $first-row;
    my $safe-count = $current-row.comb('.').elems;

    for 1 .. $total-rows - 1 -> $i {
        my $next-row = '';
        my $padded-row = '.' ~ $current-row ~ '.';
        for 0 .. $current-row.chars - 1 -> $j {
            my $pattern = $padded-row.substr($j, 3);
            if $pattern ~~ / '^^.' | '.^^' | '^..' | '..^' / {
                $next-row ~= '^';
            } else {
                $next-row ~= '.';
                $safe-count++;
            }
        }
        $current-row = $next-row;
    }
    say $safe-count;
}
