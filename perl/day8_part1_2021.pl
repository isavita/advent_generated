use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die "Cannot open file: $!";
my $count = 0;

while (my $line = <$fh>) {
	chomp $line;
	my ($input, $output) = split(' \| ', $line);
	foreach my $digit (split(' ', $output)) {
		my $length = length($digit);
		$count++ if ($length == 2 || $length == 4 || $length == 3 || $length == 7);
	}
}

print $count;