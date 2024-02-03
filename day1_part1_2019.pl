
use strict;
use warnings;

my @masses;
my $total = 0;

open(my $fh, '<', 'input.txt') or die "Cannot open file: $!";

while (my $line = <$fh>) {
    chomp $line;
    processLine($line);
}

close $fh;

sub processLine {
    my ($line) = @_;
    my $m = int($line);
    push @masses, $m;
}

sub getTotal {
    my $tempTotal = 0;

    foreach my $m (@masses) {
        $tempTotal += (int($m/3) - 2);
    }

    $total = $tempTotal;
    return;
}

getTotal();
print "$total\n";
