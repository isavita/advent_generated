
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die "Error opening file: $!";
my $mask = "";
my %mem;

while (my $line = <$fh>) {
    chomp($line);
    if ($line =~ /^mask = (.*)$/) {
        $mask = $1;
    } else {
        my ($address, $value) = $line =~ /mem\[(\d+)] = (\d+)/;
        if ($address && $value) {
            $mem{$address} = applyMask($value, $mask);
        }
    }
}

my $sum = 0;
foreach my $value (values %mem) {
    $sum += $value;
}

print "$sum\n";

sub applyMask {
    my ($value, $mask) = @_;
    my $result = 0;
    for (my $i = 0; $i < 36; $i++) {
        my $bitValue = 1 << (35 - $i);
        if (substr($mask, $i, 1) eq '1') {
            $result |= $bitValue;
        } elsif (substr($mask, $i, 1) eq 'X') {
            $result |= ($value & $bitValue);
        }
    }
    return $result;
}
