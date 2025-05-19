
sub MAIN {
    my $current-row = 'input.txt'.IO.slurp.chomp;
    my $safe-tiles = $current-row.comb('.').elems;

    for 1 .. 39 {
        my $next-row = '';
        my $padded-row = '.' ~ $current-row ~ '.';
        for 1 .. $current-row.chars -> $i {
            given $padded-row.substr($i - 1, 3) {
                when '^^.' { $next-row ~= '^'; }
                when '.^^' { $next-row ~= '^'; }
                when '^..' { $next-row ~= '^'; }
                when '..^' { $next-row ~= '^'; }
                default   { $next-row ~= '.'; }
            }
        }
        $current-row = $next-row;
        $safe-tiles += $current-row.comb('.').elems;
    }

    say $safe-tiles;
}
