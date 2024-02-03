
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die "Could not open file 'input.txt' $!";
my $input = <$fh>;
close $fh;

chomp($input);
my $halfway = length($input) / 2;
my $sum = 0;

for (my $i = 0; $i < length($input); $i++) {
    my $next = ($i + $halfway) % length($input);
    if (substr($input, $i, 1) == substr($input, $next, 1)) {
        $sum += int(substr($input, $i, 1));
    } else {
        $sum += 0;
    }
}

print "$sum\n";
