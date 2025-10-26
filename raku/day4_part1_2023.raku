
sub MAIN {
    my $total = 0;
    for 'input.txt'.IO.lines {
        my ($win, $mine) = .split(' | ');
        my $matches = $mine.words.Set ∩ $win.words.Set;
        $total += (2 ** ($matches - 1)).Int if $matches;
    }
    say $total;
}
