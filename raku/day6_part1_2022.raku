
sub MAIN {
    my $data = slurp 'input.txt'.trim;
    my $marker-length = 4;

    my $index = (0 .. $data.chars - $marker-length).first(-> $i {
        $data.substr($i, $marker-length).comb.Set.elems == $marker-length
    });

    say $index + $marker-length;
}
