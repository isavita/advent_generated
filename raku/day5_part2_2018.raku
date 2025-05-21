
#!/usr/bin/env raku

sub react($p) {
    my @s;
    for $p.comb -> $c {
        if @s.elems and $c.lc eq @s[*-1].lc and $c ne @s[*-1] {
            @s.pop;
        } else {
            @s.push($c);
        }
    }
    return @s.join;
}

sub MAIN {
    my $p = 'input.txt'.IO.slurp.trim;

    my $rp = react($p);
    say $rp.chars;

    my @u = $p.lc.comb.unique;
    my $ml = $p.chars;

    for @u -> $unit {
        my $fp = $p.comb.grep({ $_.lc ne $unit }).join;
        my $rl = react($fp).chars;
        $ml = $rl min $ml;
    }

    say $ml;
}
