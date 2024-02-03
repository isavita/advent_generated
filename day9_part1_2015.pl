
use strict;
use warnings;

my %distances = readAndParseInput("input.txt");
my @locations = getUniqueLocations(\%distances);
my $minDistance = findShortestRoute(\@locations, \%distances);
print "$minDistance\n";

sub readAndParseInput {
    my ($filename) = @_;
    open my $fh, '<', $filename or die "Error reading input: $!";
    my %distances;
    while (my $line = <$fh>) {
        my @parts = split(' ', $line);
        next if @parts != 5;

        my ($from, $to, $dist) = @parts[0, 2, 4];
        my $distance = int($dist);

        $distances{$from}->{$to} = $distance;
        $distances{$to}->{$from} = $distance; # Assuming distance is symmetric
    }
    close $fh;
    return %distances;
}

sub getUniqueLocations {
    my ($distances) = @_;
    my %locationSet;
    foreach my $from (keys %$distances) {
        $locationSet{$from} = 1;
        foreach my $to (keys %{$distances->{$from}}) {
            $locationSet{$to} = 1;
        }
    }
    return keys %locationSet;
}

sub findShortestRoute {
    my ($locations, $distances) = @_;
    my $minDistance = -1;
    permute($locations, 0, \$minDistance, $distances);
    return $minDistance;
}

sub permute {
    my ($arr, $i, $minDistance, $distances) = @_;
    if ($i > scalar(@$arr)) {
        return;
    }
    if ($i == scalar(@$arr)) {
        my $dist = calculateRouteDistance($arr, $distances);
        if ($$minDistance == -1 || $dist < $$minDistance) {
            $$minDistance = $dist;
        }
        return;
    }
    for my $j ($i .. $#{$arr}) {
        @$arr[$i, $j] = @$arr[$j, $i];
        permute($arr, $i+1, $minDistance, $distances);
        @$arr[$i, $j] = @$arr[$j, $i];
    }
}

sub calculateRouteDistance {
    my ($route, $distances) = @_;
    my $sum = 0;
    for my $i (0 .. scalar(@$route)-2) {
        $sum += $distances->{$$route[$i]}{$$route[$i+1]};
    }
    return $sum;
}
