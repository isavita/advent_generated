
sub MAIN {
    my @grid;
    my $moves = '';
    my ($rows,$cols,$rr,$rc);

    for 'input.txt'.IO.lines {
        when /^^^$$/                            { $moves ~= $_ }
        when /^ <[.#@O]>+ $$/ && !@grid.elems  { $cols = .chars }
        when /^ <[.#@O]>+ $$/                 { }
        default                                 { $moves ~= $_ }
    }

    @grid = 'input.txt'.IO.lines.grep({/^ <[.#@O]>+ $$/}).map: *.comb.Array;
    $rows = @grid.elems;
    for ^$rows -> $r {
        for ^$cols -> $c {
            ($rr,$rc) = ($r,$c) if @grid[$r][$c] eq '@';
        }
    }

    for $moves.comb -> $mv {
        my ($dr,$dc) = do given $mv {
            when '^' { (-1,0) }
            when 'v' { (1,0)  }
            when '<' { (0,-1) }
            when '>' { (0,1)  }
            default   { next }
        }
        my ($nr,$nc) = ($rr+$dr,$rc+$dc);
        next unless 0 <= $nr < $rows and 0 <= $nc < $cols;
        if @grid[$nr][$nc] eq '#' { next }
        if @grid[$nr][$nc] eq 'O' {
            my ($br,$bc) = ($nr,$nc);
            loop {
                ($br,$bc) = ($br+$dr,$bc+$dc);
                last unless 0 <= $br < $rows and 0 <= $bc < $cols;
                last if @grid[$br][$bc] eq '#';
                last if @grid[$br][$bc] eq '.';
            }
            next unless @grid[$br][$bc] eq '.';
            while ($br,$bc) ne ($nr,$nc) {
                my ($pr,$pc) = ($br-$dr,$bc-$dc);
                @grid[$br][$bc] = 'O';
                ($br,$bc) = ($pr,$pc);
            }
            @grid[$nr][$nc] = '.';
        }
        @grid[$rr][$rc] = '.';
        @grid[$nr][$nc] = '@';
        ($rr,$rc) = ($nr,$nc);
    }

    my $sum = 0;
    for ^$rows -> $r {
        for ^$cols -> $c {
            $sum += $r*100 + $c if @grid[$r][$c] eq 'O';
        }
    }
    say $sum;
}
