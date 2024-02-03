
open(my $fh, '<', 'input.txt') or die $!;
my $input = do { local $/; <$fh> };
close($fh);

my $floor = 0;
my $position = 0;
for my $i (0..length($input)-1) {
    my $c = substr($input, $i, 1);
    if ($c eq '(') {
        $floor++;
    } else {
        $floor--;
    }
    if ($floor == -1) {
        $position = $i + 1;
        last;
    }
}
print $position . "\n";
