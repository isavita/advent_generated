
open(my $fh, '<', 'input.txt') or die $!;
my $input = <$fh>;
close($fh);

my @cups = (0);
my $currentCup;
for my $i (0..length($input)-1) {
    my $cup = substr($input, $i, 1);
    if ($i == 0) {
        $currentCup = $cup;
    }
    if ($i < length($input)-1) {
        my $nextCup = substr($input, $i+1, 1);
        $cups[$cup] = $nextCup;
    }
}
my $firstCup = substr($input, 0, 1);
my $lastCup = substr($input, -1);
$cups[$lastCup] = $firstCup;

for (my $i = 0; $i < 100; $i++) {
    my $pickup1 = $cups[$currentCup];
    my $pickup2 = $cups[$pickup1];
    my $pickup3 = $cups[$pickup2];

    $cups[$currentCup] = $cups[$pickup3];

    my $destinationCup = $currentCup - 1;
    if ($destinationCup < 1) {
        $destinationCup = length($input);
    }
    while ($destinationCup == $pickup1 || $destinationCup == $pickup2 || $destinationCup == $pickup3) {
        $destinationCup--;
        if ($destinationCup < 1) {
            $destinationCup = length($input);
        }
    }

    $cups[$pickup3] = $cups[$destinationCup];
    $cups[$destinationCup] = $pickup1;

    $currentCup = $cups[$currentCup];
}

my $cup = $cups[1];
while ($cup != 1) {
    print $cup;
    $cup = $cups[$cup];
    if ($cup == 1) {
        last;
    }
}
print "\n";
