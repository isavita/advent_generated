
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die "Error opening file: $!";
my $earliestDeparture = <$fh>;
chomp($earliestDeparture);
my $busIDs = <$fh>;
chomp($busIDs);
my @busIDs = split(',', $busIDs);

my $earliestBusID = 0;
my $minWaitTime = $earliestDeparture;

foreach my $id (@busIDs) {
    if ($id eq "x") {
        next;
    }
    my $busID = $id;
    my $waitTime = $busID - ($earliestDeparture % $busID);
    if ($waitTime < $minWaitTime) {
        $minWaitTime = $waitTime;
        $earliestBusID = $busID;
    }
}

print $earliestBusID * $minWaitTime . "\n";
