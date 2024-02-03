
use strict;
use warnings;

my @freqChanges = readInput();
my $freq = 0;

foreach my $change (@freqChanges) {
    $freq += parseChange($change);
}

print "$freq\n";

sub readInput {
    my @lines;
    open(my $fh, '<', 'input.txt') or die $!;
    while (my $line = <$fh>) {
        chomp $line;
        push @lines, $line;
    }
    close $fh;
    return @lines;
}

sub parseChange {
    my ($change) = @_;
    my ($sign, $num, $err) = parseSignNum($change);
    if ($err) {
        die $err;
    }
    return $sign * $num;
}

sub parseSignNum {
    my ($change) = @_;
    my $sign = 1;
    if (substr($change, 0, 1) eq '-') {
        $sign = -1;
        $change = substr($change, 1);
    }
    my $num = int($change);
    if ($num eq '0') {
        return (0, 0, "invalid frequency change: $change");
    }
    return ($sign, $num, undef);
}
