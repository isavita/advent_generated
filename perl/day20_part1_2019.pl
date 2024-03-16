use strict;
use warnings;

my %grid;
my ($xMax, $yMax) = (0, 0);

open my $fh, '<', 'input.txt' or die $!;

my $i = 0;
while (my $line = <$fh>) {
	chomp $line;

	if (length($line) > $yMax) {
		$yMax = length($line);
	}

	for my $j (0..length($line)-1) {
		$grid{"$i $j"} = substr($line, $j, 1);
	}
	$i++;
}
$xMax = $i;

my ($aa, $zz);
my %isOuter;
my %portalName;
my %teleport;
my %cache;

for my $i (0..$xMax-1) {
	for my $j (0..$yMax-1) {
		my $c = $grid{"$i $j"};

		next unless $c =~ /[A-Z]/;

		my ($pName, $pPoint, $ok) = extractPortal($i, $j);

		next unless $ok;

		$portalName{"$pPoint"} = $pName;

		if ($pName eq "AA") {
			$aa = $pPoint;
			$isOuter{"$pPoint"} = 1;
			next;
		}

		if ($pName eq "ZZ") {
			$zz = $pPoint;
			$isOuter{"$pPoint"} = 1;
			next;
		}

		if (exists $cache{$pName}) {
			$teleport{"$pPoint"} = $cache{$pName};
			$teleport{$cache{$pName}} = $pPoint;
		} else {
			$cache{$pName} = $pPoint;
		}

		if ($j == 0 || $i == 0 || $i == $xMax-2 || $j == $yMax-2) {
			$isOuter{"$pPoint"} = 1;
		} else {
			$isOuter{"$pPoint"} = 0;
		}
	}
}

sub extractPortal {
	my ($i, $j) = @_;

	my $c1 = $grid{"$i $j"};

	if (my $c2 = $grid{"$i ".($j+1)}) {
		if ($c2 =~ /[A-Z]/) {
			my $portalName = $c1 . $c2;

			my $portalPoint = "$i ".($j+2);
			if ($grid{$portalPoint} eq '.') {
				return ($portalName, $portalPoint, 1);
			}

			$portalPoint = "$i ".($j-1);
			if ($grid{$portalPoint} eq '.') {
				return ($portalName, $portalPoint, 1);
			}
		}
	}

	if (my $c2 = $grid{($i+1)." $j"}) {
		if ($c2 =~ /[A-Z]/) {
			my $portalName = $c1 . $c2;

			my $portalPoint = ($i+2)." $j";
			if ($grid{$portalPoint} eq '.') {
				return ($portalName, $portalPoint, 1);
			}

			$portalPoint = ($i-1)." $j";
			if ($grid{$portalPoint} eq '.') {
				return ($portalName, $portalPoint, 1);
			}
		}
	}

	return ("", "", 0);
}

my $depth = 0;
my %discovered;
my @toDo;

$discovered{"$aa"} = 1;
push @toDo, $aa;

while (@toDo) {
	my $levelSize = scalar @toDo;
	for (my $i = 0; $i < $levelSize; $i++) {
		my $curr = shift @toDo;

		if ($curr eq "$zz") {
			print "$depth\n";
			exit;
		}

		my ($x, $y) = split ' ', $curr;

		my @neighbours = ("$x ".($y+1), ($x+1)." $y", "$x ".($y-1), ($x-1)." $y");

		foreach my $n (@neighbours) {
			my ($nx, $ny) = split ' ', $n;

			next if ($grid{"$nx $ny"} eq '#');

			if ($grid{"$nx $ny"} eq '.') {
				unless ($discovered{"$nx $ny"}) {
					$discovered{"$nx $ny"} = 1;
					push @toDo, "$nx $ny";
				}
			} elsif ($grid{"$nx $ny"} =~ /[A-Z]/) {
				my $next = $teleport{"$x $y"};

				unless ($discovered{"$next"}) {
					$discovered{"$next"} = 1;
					push @toDo, $next;
				}
			}
		}
	}

	$depth++;
}

print "-1\n";