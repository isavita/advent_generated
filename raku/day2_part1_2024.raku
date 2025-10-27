
sub MAIN {
    my $safe = 0;
    for 'input.txt'.IO.lines {
        my @l = .comb(/ '-'? \d+ /)».Int;
        ++$safe if is-safe(@l);
    }
    say $safe;
}

sub is-safe(@l) {
    return False if @l.elems < 2;
    my $inc = @l[1] > @l[0];
    for 0..^(@l-1) -> $i {
        my $d = @l[$i+1] - @l[$i];
        return False if $d == 0;
        return False if $inc ?? $d <= 0 !! $d >= 0;
        return False if $d.abs < 1 || $d.abs > 3;
    }
    True
}
