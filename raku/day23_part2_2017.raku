
sub MAIN {
    my $b = 57 * 100 + 100000;
    my $c = $b + 17000;
    say ($b, $b + 17 ... $c).grep({ !.is-prime }).elems;
}

