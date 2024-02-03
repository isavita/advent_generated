
open my $fh, '<', 'input.txt' or die "Cannot open file: $!";
my $steps = <$fh>;
close $fh;

chomp $steps;
my $currentPos = 0;
my $valueAfterZero = 0;

for (my $i = 1; $i <= 50000000; $i++) {
    $currentPos = ($currentPos + $steps) % $i;
    $valueAfterZero = $i if $currentPos == 0;
    $currentPos++;
}

print "$valueAfterZero\n";
