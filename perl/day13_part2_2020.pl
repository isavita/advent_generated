
use strict;
use warnings;

sub readBusIDs {
    my $fileName = shift;
    open(my $fh, '<', $fileName) or die "Cannot open file: $!";
    my $line = <$fh>; # Skip the first line
    $line = <$fh>; # Read the second line
    chomp($line);
    my @busData = split(',', $line);

    my @ids;
    my @offsets;
    for my $i (0..$#busData) {
        my $bus = $busData[$i];
        if ($bus ne 'x') {
            push @ids, $bus;
            push @offsets, $i;
        }
    }
    close($fh);
    return (\@ids, \@offsets);
}

sub extendedGCD {
    my ($a, $b) = @_;
    if ($a == 0) {
        return (0, 1);
    }
    my ($x1, $y1) = extendedGCD($b % $a, $a);
    my $x = $y1 - int($b / $a) * $x1;
    my $y = $x1;
    return ($x, $y);
}

sub findEarliestTimestamp {
    my ($ids, $offsets) = @_;
    my $N = 1;
    $N *= $_ for @$ids;

    my $result = 0;
    for my $i (0..$#$ids) {
        my $id = $ids->[$i];
        my $ni = int($N / $id);
        my ($xi, $yi) = extendedGCD($ni, $id);
        $result += (-$offsets->[$i] + $id) % $id * $xi * $ni;
    }
    return $result % $N;
}

my ($ids, $offsets) = readBusIDs("input.txt");
my $timestamp = findEarliestTimestamp($ids, $offsets);
print "$timestamp\n";
