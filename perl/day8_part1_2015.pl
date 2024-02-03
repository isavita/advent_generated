
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die "Error opening file: $!";
my $totalDiff = 0;

while (my $line = <$fh>) {
    chomp $line;
    my $codeLength = length($line);
    my $memoryLength = calculateMemoryLength($line);
    $totalDiff += $codeLength - $memoryLength;
}

close($fh);

print "$totalDiff\n";

sub calculateMemoryLength {
    my $s = shift;
    my $length = 0;
    my $inEscape = 0;
    my $hexCount = 0;

    for (my $i = 1; $i < length($s) - 1; $i++) {
        if ($hexCount > 0) {
            $hexCount--;
        } elsif ($inEscape) {
            if (substr($s, $i, 1) eq 'x') {
                $hexCount = 2;
            }
            $inEscape = 0;
            $length++;
        } elsif (substr($s, $i, 1) eq '\\') {
            $inEscape = 1;
        } else {
            $length++;
        }
    }

    return $length;
}
