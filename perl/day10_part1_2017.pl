
open(my $fh, '<', 'input.txt') or die "Cannot open file: $!";
my $input = <$fh>;
close($fh);

my @lengths = split(',', $input);
my @list = (0..255);
my $currentPosition = 0;
my $skipSize = 0;

foreach my $length (@lengths) {
    for (my $i = 0; $i < $length/2; $i++) {
        my $start = ($currentPosition + $i) % 256;
        my $end = ($currentPosition + $length - 1 - $i) % 256;
        ($list[$start], $list[$end]) = ($list[$end], $list[$start]);
    }
    $currentPosition = ($currentPosition + $length + $skipSize) % 256;
    $skipSize++;
}

my $result = $list[0] * $list[1];
print "$result\n";
