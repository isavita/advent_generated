
use strict;
use warnings;

sub itemPriority {
    my ($item) = @_;
    if ($item ge 'a' && $item le 'z') {
        return ord($item) - ord('a') + 1;
    } else {
        return ord($item) - ord('A') + 27;
    }
}

open my $fh, '<', 'input.txt' or die "Error opening file: $!";
my $sum = 0;

while (my $line = <$fh>) {
    chomp $line;
    my $half = length($line) / 2;
    my $firstCompartment = substr($line, 0, $half);
    my $secondCompartment = substr($line, $half);

    my %compartmentMap;
    foreach my $item (split //, $firstCompartment) {
        $compartmentMap{$item}++;
    }
    foreach my $item (split //, $secondCompartment) {
        if (exists $compartmentMap{$item}) {
            $sum += itemPriority($item);
            last;
        }
    }
}

close $fh or die "Error closing file: $!";
print "$sum\n";
