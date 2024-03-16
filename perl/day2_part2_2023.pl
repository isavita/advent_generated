use strict;
use warnings;

open my $fh, '<', 'input.txt' or die $!;
my $regex = qr/Game (\d+): (.+)/;
my $cubeRegex = qr/(\d+) (red|green|blue)/;
my $totalPower = 0;

while (my $line = <$fh>) {
	chomp $line;
	if (my ($gameNum, $rounds) = $line =~ $regex) {
		my @rounds = split(';', $rounds);
		my ($maxRed, $maxGreen, $maxBlue) = (0, 0, 0);

		foreach my $round (@rounds) {
			my @cubes = $round =~ /$cubeRegex/g;
			my ($red, $green, $blue) = (0, 0, 0);

			for (my $i = 0; $i < @cubes; $i += 2) {
				my $count = $cubes[$i];
				my $color = $cubes[$i + 1];
				if ($color eq 'red') {
					$red += $count;
				} elsif ($color eq 'green') {
					$green += $count;
				} elsif ($color eq 'blue') {
					$blue += $count;
				}
			}

			$maxRed = $red if $red > $maxRed;
			$maxGreen = $green if $green > $maxGreen;
			$maxBlue = $blue if $blue > $maxBlue;
		}

		my $power = $maxRed * $maxGreen * $maxBlue;
		$totalPower += $power;
	}
}

close $fh;

print "$totalPower\n";