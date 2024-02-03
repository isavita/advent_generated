
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die $!;
my $input = <$fh>;
close($fh);

my @repeatedInput = repeatInput($input, 10000);
my $offset = substr($input, 0, 7);

for (my $phase = 0; $phase < 100; $phase++) {
    my $sum = 0;
    for (my $i = scalar(@repeatedInput) - 1; $i >= $offset; $i--) {
        $sum += $repeatedInput[$i];
        $repeatedInput[$i] = $sum % 10;
    }
}

for (my $i = $offset; $i < $offset + 8; $i++) {
    print $repeatedInput[$i];
}
print "\n";

sub repeatInput {
    my ($input, $times) = @_;
    my @digits;
    for (my $t = 0; $t < $times; $t++) {
        my @inputDigits = split('', $input);
        for (my $i = 0; $i < scalar(@inputDigits); $i++) {
            $digits[$t * scalar(@inputDigits) + $i] = $inputDigits[$i];
        }
    }
    return @digits;
}
