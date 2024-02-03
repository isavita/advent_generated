
use strict;
use warnings;

my $preambleLength = 25;
my @numbers;

open(my $fh, "<", "input.txt") or die "Error opening file: $!";
while (my $line = <$fh>) {
    chomp $line;
    push @numbers, $line;
}
close($fh);

for my $i ($preambleLength..$#numbers) {
    my $number = $numbers[$i];
    my @previousNumbers = @numbers[$i-$preambleLength..$i-1];
    if (!isValid($number, \@previousNumbers)) {
        print "$number\n";
        last;
    }
}

sub isValid {
    my ($number, $previousNumbers) = @_;
    my %seen;
    foreach my $n (@$previousNumbers) {
        if ($seen{$number-$n}) {
            return 1;
        }
        $seen{$n} = 1;
    }
    return 0;
}
