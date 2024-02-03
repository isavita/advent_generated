
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die $!;
my @values;
while (my $line = <$fh>) {
    chomp $line;
    push @values, $line;
}
close($fh);

sub filterValues {
    my ($values, $criteria) = @_;
    for (my $i = 0; $i < length($values->[0]); $i++) {
        my ($zeros, $ones) = (0, 0);
        foreach my $val (@$values) {
            if (substr($val, $i, 1) eq '0') {
                $zeros++;
            } else {
                $ones++;
            }
        }
        my $keep = $criteria->($zeros, $ones);
        $values = filterByBit($values, $i, $keep);
        last if (@$values == 1);
    }
    return $values->[0];
}

sub filterByBit {
    my ($values, $bitIndex, $keep) = @_;
    my @filtered;
    foreach my $val (@$values) {
        if (substr($val, $bitIndex, 1) eq $keep) {
            push @filtered, $val;
        }
    }
    return \@filtered;
}

my $oxygenGeneratorRating = filterValues(\@values, sub {
    my ($zeros, $ones) = @_;
    if ($zeros > $ones) {
        return '0';
    } else {
        return '1';
    }
});
my $oxygenGeneratorRatingInt = oct("0b$oxygenGeneratorRating");

my $co2ScrubberRating = filterValues(\@values, sub {
    my ($zeros, $ones) = @_;
    if ($zeros <= $ones) {
        return '0';
    } else {
        return '1';
    }
});
my $co2ScrubberRatingInt = oct("0b$co2ScrubberRating");

print $oxygenGeneratorRatingInt * $co2ScrubberRatingInt;
