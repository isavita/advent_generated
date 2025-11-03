
sub MAIN {
    my @layout  = 'input.txt'.IO.lines».comb;
    my \rows  = +@layout;
    my \cols  = +@layout[0];

    while 1 {
        my @next;
        my int $changes = 0;

        for ^rows -> \r {
            @next[r] = [];
            for ^cols -> \c {
                my $cell = @layout[r][c];
                if $cell eq 'L' {
                    my int $occ = 0;
                    for -1,0,1 -> \dr {
                        for -1,0,1 -> \dc {
                            next if dr == 0 && dc == 0;
                            my int $nr = r + dr;
                            my int $nc = c + dc;
                            next unless 0 <= $nr < rows and 0 <= $nc < cols;
                            $occ++ if @layout[$nr][$nc] eq '#';
                        }
                    }
                    @next[r][c] = $occ == 0 ?? '#' !! 'L';
                    $changes++ if @next[r][c] ne $cell;
                }
                elsif $cell eq '#' {
                    my int $occ = 0;
                    for -1,0,1 -> \dr {
                        for -1,0,1 -> \dc {
                            next if dr == 0 && dc == 0;
                            my int $nr = r + dr;
                            my int $nc = c + dc;
                            next unless 0 <= $nr < rows and 0 <= $nc < cols;
                            $occ++ if @layout[$nr][$nc] eq '#';
                        }
                    }
                    @next[r][c] = $occ >= 4 ?? 'L' !! '#';
                    $changes++ if @next[r][c] ne $cell;
                }
                else {
                    @next[r][c] = $cell;
                }
            }
        }

        last if $changes == 0;
        @layout = @next;
    }

    say [+] @layout.map: { .grep('#').elems }
}
