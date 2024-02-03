
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die "Error reading file: $!";
my %firewall;

while (my $line = <$fh>) {
    chomp $line;
    my @fields = split(": ", $line);
    my $depth = $fields[0];
    my $rng = $fields[1];
    $firewall{$depth} = { Range => $rng, Position => 0, Direction => 1 };
}

my $delay = 0;
while (1) {
    if (passThrough(\%firewall, $delay)) {
        last;
    }
    $delay++;
}

print "$delay\n";

sub passThrough {
    my ($firewall, $delay) = @_;
    foreach my $depth (keys %$firewall) {
        my $scanner = $firewall->{$depth};
        if (($depth + $delay) % (2 * ($scanner->{Range} - 1)) == 0) {
            return 0;
        }
    }
    return 1;
}
