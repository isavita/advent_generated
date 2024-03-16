open my $fh, '<', "input.txt" or die $!;
my $initialState;
my %rules;

while (my $line = <$fh>) {
	chomp $line;
	if ($line =~ /initial state/) {
		$initialState = (split /: /, $line)[1];
	} elsif ($line =~ /=>/) {
		my @parts = split / => /, $line;
		$rules{$parts[0]} = substr($parts[1], 0, 1);
	}
}

my %state;
foreach my $i (0..length($initialState)-1) {
	$state{$i} = '#' if substr($initialState, $i, 1) eq '#';
}

my $previousPattern = "";
my $previousSum = 0;
my $offset = 0;
for (my $generation = 0; $generation < 50000000000; $generation++) {
	my %newState;
	my ($minPot, $maxPot) = minMaxKeys(%state);
	for my $i ($minPot - 2 .. $maxPot + 2) {
		my $pattern = "";
		foreach my $j ($i - 2 .. $i + 2) {
			$pattern .= $state{$j} eq '#' ? "#" : ".";
		}
		$newState{$i} = '#' if $rules{$pattern} eq '#';
	}
	%state = %newState;

	my ($currentPattern, $currentSum) = statePattern(%state);
	if ($currentPattern eq $previousPattern) {
		$offset = $currentSum - $previousSum;
		my $remainingGenerations = 50000000000 - $generation - 1;
		my $finalSum = $currentSum + $offset * $remainingGenerations;
		print "$finalSum\n";
		last;
	}
	$previousPattern = $currentPattern;
	$previousSum = $currentSum;
}

sub minMaxKeys {
	my %m = @_;
	my ($minKey, $maxKey);
	my $first = 1;
	foreach my $k (keys %m) {
		if ($first) {
			$minKey = $k;
			$maxKey = $k;
			$first = 0;
		} else {
			$minKey = $k if $k < $minKey;
			$maxKey = $k if $k > $maxKey;
		}
	}
	return ($minKey, $maxKey);
}

sub statePattern {
	my %m = @_;
	my ($minPot, $maxPot) = minMaxKeys(%m);
	my $pattern = "";
	my $sum = 0;
	foreach my $i ($minPot .. $maxPot) {
		if ($m{$i} eq '#') {
			$pattern .= "#";
			$sum += $i;
		} else {
			$pattern .= ".";
		}
	}
	return ($pattern, $sum);
}