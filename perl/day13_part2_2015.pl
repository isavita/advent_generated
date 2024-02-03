
use strict;
use warnings;

my %happinessMap = readHappinessValues("input.txt");
addYourself(\%happinessMap);

my @guests = getGuestList(\%happinessMap);
my $maxHappiness = calculateOptimalArrangement(\@guests, \%happinessMap);
print "$maxHappiness\n";

sub readHappinessValues {
    my ($filename) = @_;
    open(my $fh, '<', $filename) or die "Error reading input: $!";
    
    my %happinessMap;
    while (my $line = <$fh>) {
        my @parts = split(' ', $line);
        next if scalar @parts < 11;
        my ($from, $to) = ($parts[0], substr($parts[10], 0, -1));
        my $change = $parts[3];
        $change = -$change if $parts[2] eq "lose";

        $happinessMap{$from} //= {};
        $happinessMap{$from}{$to} = $change;
    }
    
    close($fh);
    return %happinessMap;
}

sub addYourself {
    my ($happinessMap) = @_;
    $happinessMap->{"You"} = {};
    foreach my $guest (keys %$happinessMap) {
        $happinessMap->{$guest}{"You"} = 0;
        $happinessMap->{"You"}{$guest} = 0;
    }
}

sub getGuestList {
    my ($happinessMap) = @_;
    return keys %$happinessMap;
}

sub calculateOptimalArrangement {
    my ($guests, $happinessMap) = @_;
    my $maxHappiness = 0;
    permute($guests, 0, \$maxHappiness, $happinessMap);
    return $maxHappiness;
}

sub permute {
    my ($arr, $i, $maxHappiness, $happinessMap) = @_;
    if ($i > scalar @$arr) {
        return;
    }
    if ($i == scalar @$arr) {
        my $happiness = calculateHappiness($arr, $happinessMap);
        if ($happiness > $$maxHappiness) {
            $$maxHappiness = $happiness;
        }
        return;
    }
    for my $j ($i..$#$arr) {
        @$arr[$i, $j] = @$arr[$j, $i];
        permute($arr, $i+1, $maxHappiness, $happinessMap);
        @$arr[$i, $j] = @$arr[$j, $i];
    }
}

sub calculateHappiness {
    my ($arrangement, $happinessMap) = @_;
    my $happiness = 0;
    my $n = scalar @$arrangement;
    for my $i (0..$n-1) {
        my $left = ($i + $n - 1) % $n;
        my $right = ($i + 1) % $n;
        $happiness += $happinessMap->{$arrangement->[$i]}{$arrangement->[$left]};
        $happiness += $happinessMap->{$arrangement->[$i]}{$arrangement->[$right]};
    }
    return $happiness;
}
