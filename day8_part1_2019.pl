
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die $!;
my $imageData = do { local $/; <$fh> };
close($fh);

my ($width, $height) = (25, 6);
my $layerSize = $width * $height;

my $minZeros = $layerSize + 1;
my $result = 0;

for (my $i = 0; $i < length($imageData); $i += $layerSize) {
    my $layer = substr($imageData, $i, $layerSize);
    my ($zeroCount, $oneCount, $twoCount) = (0, 0, 0);

    for my $pixel (split('', $layer)) {
        if ($pixel eq '0') {
            $zeroCount++;
        } elsif ($pixel eq '1') {
            $oneCount++;
        } else {
            $twoCount++;
        }
    }

    if ($zeroCount < $minZeros) {
        $minZeros = $zeroCount;
        $result = $oneCount * $twoCount;
    }
}

print "$result\n";
