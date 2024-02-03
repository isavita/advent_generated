
use strict;
use warnings;

open my $fh, '<', 'input.txt' or die $!;
my @instructions;
while (my $line = <$fh>) {
	chomp $line;
	@instructions = split(', ', $line);
}

my %visited;
my %directions = (0 => [0, 1], 1 => [1, 0], 2 => [0, -1], 3 => [-1, 0]);
my $dirIndex = 0;
my @pos = (0, 0);
$visited{"0,0"} = 1;

foreach my $instruction (@instructions) {
	my $turn = substr($instruction, 0, 1);
	my $blocks = substr($instruction, 1);

	if ($turn eq "R") {
		$dirIndex = ($dirIndex + 1) % 4;
	} else {
		$dirIndex = ($dirIndex - 1 + 4) % 4;
	}

	for (my $i = 0; $i < $blocks; $i++) {
		$pos[0] += $directions{$dirIndex}[0];
		$pos[1] += $directions{$dirIndex}[1];

		my $key = join(",", @pos);
		if ($visited{$key}) {
			print abs($pos[0]) + abs($pos[1]) . "\n";
			exit;
		}
		$visited{$key} = 1;
	}
}

print "-1\n";

sub abs {
	my $x = shift;
	return $x < 0 ? -$x : $x;
}
