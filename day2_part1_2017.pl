
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die "Could not open file 'input.txt' $!";
my @lines = <$fh>;
close($fh);

my $checksum = 0;

foreach my $line (@lines) {
    my @nums = split(' ', $line);
    my $minVal = $nums[0];
    my $maxVal = $nums[0];

    foreach my $numStr (@nums) {
        my $num = $numStr;
        if ($num < $minVal) {
            $minVal = $num;
        }
        if ($num > $maxVal) {
            $maxVal = $num;
        }
    }

    $checksum += ($maxVal - $minVal);
}

print $checksum;
