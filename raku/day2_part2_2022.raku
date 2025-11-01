
sub MAIN() {
    my $total = 0;
    for 'input.txt'.IO.lines {
        my ($o, $r) = .comb: / \S /;
        my $m = $r eq 'X' ?? ($o eq 'A' ?? 'Z' !! $o eq 'B' ?? 'X' !! 'Y')
              !! $r eq 'Y' ?? ($o eq 'A' ?? 'X' !! $o eq 'B' ?? 'Y' !! 'Z')
              !!              ($o eq 'A' ?? 'Y' !! $o eq 'B' ?? 'Z' !! 'X');
        $total += (1, 2, 3)[$m eq 'X' ?? 0 !! $m eq 'Y' ?? 1 !! 2];
        $total += 6 if ($o eq 'A' && $m eq 'Y') || ($o eq 'B' && $m eq 'Z') || ($o eq 'C' && $m eq 'X');
        $total += 3 if ($o eq 'A' && $m eq 'X') || ($o eq 'B' && $m eq 'Y') || ($o eq 'C' && $m eq 'Z');
    }
    say $total;
}
