
sub MAIN {
    my %cubes = red => 12, green => 13, blue => 14;
    my $sum = 0;
    for 'input.txt'.IO.lines.kv -> $i, $line {
        my $valid = True;
        for $line.split(': ')[1].split('; ') -> $set {
            for $set.split(', ') -> $draw {
                my ($n, $color) = $draw.split(' ');
                $valid = False if +$n > %cubes{$color};
            }
        }
        $sum += $i + 1 if $valid;
    }
    say $sum;
}
