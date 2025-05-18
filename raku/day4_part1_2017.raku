
sub MAIN {
    my $valid-count = 'input.txt'.IO.lines.grep(-> $line {
        $line.words.elems == $line.words.Set.elems
    }).elems;
    say $valid-count;
}
