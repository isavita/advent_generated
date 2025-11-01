
sub item-priority($item) {
    $item ~~ / <[a..z]> / ?? $item.ord - 'a'.ord + 1 !! $item.ord - 'A'.ord + 27
}

sub MAIN {
    my $sum = 0;
    for 'input.txt'.IO.lines {
        my $half = .chars div 2;
        my $first = .substr(0, $half);
        my $second = .substr($half);
        my $common = ($first.comb ∩ $second.comb).keys[0];
        $sum += item-priority($common);
    }
    say $sum;
}
