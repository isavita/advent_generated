
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die $!;
my $sum = 0;

while (my $line = <$fh>) {
    chomp $line;
    my ($firstDigit, $lastDigit) = findFirstAndLastDigit($line);
    $sum += 10*$firstDigit + $lastDigit;
}

close($fh);

print "$sum\n";

sub findFirstAndLastDigit {
    my ($line) = @_;
    my @digits = qw(zero one two three four five six seven eight nine);

    my ($firstDigit, $lastDigit) = (0, 0);
    for my $i (0..length($line)-1) {
        my $char = substr($line, $i, 1);
        if ($char =~ /\d/) {
            if ($firstDigit == 0) {
                $firstDigit = $char;
            }
            $lastDigit = $char;
        } else {
            foreach my $j (0..$#digits) {
                if (index($line, $digits[$j], $i) == $i) {
                    if ($firstDigit == 0) {
                        $firstDigit = $j;
                    }
                    $lastDigit = $j;
                    last;
                }
            }
        }
    }

    return ($firstDigit, $lastDigit);
}
