
sub MAIN {
    say 'input.txt'.IO.lines.grep( -> $line {
        my ($policy, $password) = $line.split(': ');
        my ($positions, $letter) = $policy.split(' ');
        my ($pos1, $pos2) = $positions.split('-')».Int;

        my $char1 = $password.substr($pos1 - 1, 1);
        my $char2 = $password.substr($pos2 - 1, 1);

        ($char1 eq $letter) ^^ ($char2 eq $letter)
    }).elems;
}
