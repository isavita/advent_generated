
open(my $fh, '<', 'input.txt') or die "Cannot open file: $!";
my $input = <$fh>;
close($fh);

chomp($input);
my $sum = 0;

for(my $i = 0; $i < length($input); $i++) {
    my $next = ($i + 1) % length($input);
    if (substr($input, $i, 1) == substr($input, $next, 1)) {
        $sum += ord(substr($input, $i, 1)) - ord('0');
    } else {
        $sum += 0;
    }
}

print "$sum\n";
