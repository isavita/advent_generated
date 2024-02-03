
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die $!;

my $data = do { local $/; <$fh> };

my ($row, $column) = ($data =~ /row (\d+), column (\d+)/);

die "Invalid input format." unless defined $row && defined $column;

my $pos = ($row + $column - 2) * ($row + $column - 1) / 2 + $column;

my $code = getCode($pos);

print "$code\n";

sub getCode {
    my $position = shift;
    my $startCode = 20151125;
    my $multiplier = 252533;
    my $modulus = 33554393;

    my $code = $startCode;
    for (my $i = 1; $i < $position; $i++) {
        $code = ($code * $multiplier) % $modulus;
    }
    return $code;
}
