
sub find_marker(Str $data, Int $marker_length) {
    for $marker_length .. $data.chars - 1 -> $i {
        if $data.substr($i - $marker_length, $marker_length).comb.unique.elems == $marker_length {
            return $i;
        }
    }
}

sub MAIN {
    my $data = 'input.txt'.IO.slurp.trim;
    say find_marker($data, 14);
}
