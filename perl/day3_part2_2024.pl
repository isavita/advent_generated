
use strict;
use warnings;

my $input = do { open my $fh, '<', 'input.txt' or die $!; local $/; <$fh> };

my $re = qr/(mul\(\d{1,3},\d{1,3}\))|(do\(\))|(don't\(\))/;

my $enabled = 1;
my $totalSum = 0;

while ($input =~ /$re/g) {
    if ($1) {
        if ($enabled) {
            my ($x, $y) = split ',', substr($1, 4, length($1) - 5);
            $totalSum += $x * $y;
        }
    } elsif ($2) {
        $enabled = 1;
    } elsif ($3) {
        $enabled = 0;
    }
}

print $totalSum, "\n";
