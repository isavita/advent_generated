
use strict;
use warnings;

sub processLine {
    my $line = shift;
    my $n = int($line);
    return $n;
}

sub calcFuelMass {
    my $mass = shift;
    my $fuel = int($mass / 3) - 2;
    if ($fuel <= 0) {
        return 0;
    }
    return $fuel + calcFuelMass($fuel);
}

sub getTotal {
    my @masses = @_;
    my $total = 0;
    foreach my $mass (@masses) {
        $total += calcFuelMass($mass);
    }
    return $total;
}

open(my $fh, '<', 'input.txt') or die "Cannot open file: $!";
my @masses;
while (my $line = <$fh>) {
    chomp $line;
    my $n = processLine($line);
    push @masses, $n;
}
close($fh);

my $total = getTotal(@masses);
print "$total\n";
