
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die "Error opening file: $!";
my $sum = 0;

while (my $line = <$fh>) {
    chomp($line);
    next if $line eq "";

    my ($firstDigit, $lastDigit) = (-1, -1);

    foreach my $char (split //, $line) {
        if ($char =~ /\d/) {
            $firstDigit = $char - '0' if $firstDigit == -1;
            $lastDigit = $char - '0';
        }
    }

    if ($firstDigit != -1 && $lastDigit != -1) {
        my $value = $firstDigit * 10 + $lastDigit;
        $sum += $value;
    }
}

close($fh) or die "Error closing file: $!";

print "$sum\n";
