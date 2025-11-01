
sub MAIN {
    my @grid = 'input.txt'.IO.lines;
    my $rows = +@grid;

    for ^@grid[0].chars -> $col {
        my $empty = -1;
        for ^$rows -> $row {
            given @grid[$row].substr($col,1) {
                when 'O' {
                    if $empty >= 0 {
                        @grid[$row].substr-rw($col,1) = '.';
                        @grid[$empty].substr-rw($col,1) = 'O';
                        ++$empty;
                    }
                }
                when '.' { $empty = $row if $empty < 0; }
                default  { $empty = -1; }
            }
        }
    }

    say [+] @grid.kv.map: -> $r, $line { ($rows - $r) * $line.comb.grep('O').elems }
}
