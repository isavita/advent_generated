
use strict;
use warnings;

my %firewall;
open(my $fh, '<', 'input.txt') or die "Error reading file: $!";
while (my $line = <$fh>) {
    chomp $line;
    my ($depth, $rng) = split /: /, $line;
    $firewall{$depth} = {Range => $rng, Position => 0, Direction => 1};
}
close($fh);

my $severity = 0;

for my $depth (0..maxDepth(\%firewall)) {
    if (exists $firewall{$depth}) {
        if ($firewall{$depth}{Position} == 0) {
            $severity += $depth * $firewall{$depth}{Range};
        }
    }

    for my $scanner (values %firewall) {
        moveScanner($scanner);
    }
}

print "$severity\n";

sub maxDepth {
    my ($firewall) = @_;
    my $max = 0;
    foreach my $depth (keys %$firewall) {
        $max = $depth if $depth > $max;
    }
    return $max;
}

sub moveScanner {
    my ($scanner) = @_;
    if ($scanner->{Position} == 0) {
        $scanner->{Direction} = 1;
    } elsif ($scanner->{Position} == $scanner->{Range}-1) {
        $scanner->{Direction} = -1;
    }
    $scanner->{Position} += $scanner->{Direction};
}
