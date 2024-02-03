
use strict;
use warnings;

sub isValidPassword {
    my $password = shift;
    my $s = "$password";
    my $hasDouble = 0;

    for (my $i = 0; $i < length($s) - 1; $i++) {
        if (substr($s, $i, 1) > substr($s, $i + 1, 1)) {
            return 0;
        }
        if (substr($s, $i, 1) == substr($s, $i + 1, 1)) {
            if (($i == 0 || substr($s, $i, 1) != substr($s, $i - 1, 1)) && ($i + 2 >= length($s) || substr($s, $i, 1) != substr($s, $i + 2, 1))) {
                $hasDouble = 1;
            }
        }
    }

    return $hasDouble;
}

open(my $fh, "<", "input.txt") or die "Cannot open file: $!";
my $rangeStr = <$fh>;
close($fh);

chomp($rangeStr);
my @ranges = split("-", $rangeStr);
my $start = $ranges[0];
my $end = $ranges[1];

my $count = 0;
for (my $i = $start; $i <= $end; $i++) {
    if (isValidPassword($i)) {
        $count++;
    }
}

print "$count\n";
