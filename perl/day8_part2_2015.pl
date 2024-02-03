
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die "Error opening file: $!";
my $totalDiff = 0;

while (my $line = <$fh>) {
    chomp($line);
    my $originalLength = length($line);
    my $encodedLength = calculateEncodedLength($line);
    $totalDiff += $encodedLength - $originalLength;
}

close($fh);

print "$totalDiff\n";

sub calculateEncodedLength {
    my ($s) = @_;
    my $encoded = "\"";
    foreach my $ch (split //, $s) {
        if ($ch eq '\\' || $ch eq '"') {
            $encoded .= "\\";
        }
        $encoded .= $ch;
    }
    $encoded .= "\"";
    return length($encoded);
}
