sub MAIN() {
    my $valid-passwords = 0;
    for 'input.txt'.IO.lines -> $line {
        my ($policy, $password) = $line.trim.split(': ');
        my ($limits, $letter) = $policy.split(' ');
        my ($min-count, $max-count) = $limits.split('-').map(*.Int);
        my $count = ($password ~~ m:g/$letter/).elems;
        if $min-count <= $count <= $max-count {
            $valid-passwords++;
        }
    }
    say $valid-passwords;
}