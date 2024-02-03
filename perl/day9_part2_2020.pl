
use strict;
use warnings;

my $invalidNumber = 14360655;
my @numbers;

open(my $fh, '<', 'input.txt') or die "Error opening file: $!";
while (my $line = <$fh>) {
    chomp $line;
    push @numbers, $line;
}
close($fh);

for my $i (0 .. $#numbers) {
    my $sum = $numbers[$i];
    my $min = $numbers[$i];
    my $max = $numbers[$i];
    for my $j ($i + 1 .. $#numbers) {
        $sum += $numbers[$j];
        if ($numbers[$j] < $min) {
            $min = $numbers[$j];
        }
        if ($numbers[$j] > $max) {
            $max = $numbers[$j];
        }
        if ($sum == $invalidNumber) {
            print $min + $max . "\n";
            exit;
        } else {
            last if $sum > $invalidNumber;
        }
    }
}
