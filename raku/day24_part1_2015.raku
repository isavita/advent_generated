
sub MAIN {
    my @packages = 'input.txt'.IO.slurp.lines.map(*.Int);
    my $target-weight = ([+] @packages) / 3;

    my $best-qe = Inf;

    for 1 .. @packages.elems -> $k {
        my $found-current-length = False;
        for @packages.combinations($k) -> @group {
            if ([+] @group) == $target-weight {
                $found-current-length = True;
                $best-qe min= ([*] @group);
            }
        }
        last if $found-current-length;
    }

    say $best-qe;
}
