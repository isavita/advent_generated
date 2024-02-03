
use strict;
use warnings;

my $totalCups = 1000000;
my $totalMoves = 10000000;

open(my $fh, '<', 'input.txt') or die $!;

my $input = <$fh>;
chomp $input;

my @cups = (0) x ($totalCups + 1);
my $lastCup;

for my $i (0 .. length($input) - 1) {
    my $cup = substr($input, $i, 1);
    if ($i > 0) {
        $cups[$lastCup] = $cup;
    }
    $lastCup = $cup;
}

for my $i (length($input) + 1 .. $totalCups) {
    $cups[$lastCup] = $i;
    $lastCup = $i;
}
$cups[$lastCup] = substr($input, 0, 1);

my $currentCup = substr($input, 0, 1);
for my $i (0 .. $totalMoves - 1) {
    my $pickup1 = $cups[$currentCup];
    my $pickup2 = $cups[$pickup1];
    my $pickup3 = $cups[$pickup2];

    $cups[$currentCup] = $cups[$pickup3];

    my $destinationCup = $currentCup - 1;
    $destinationCup = $totalCups if $destinationCup == 0;

    while ($destinationCup == $pickup1 || $destinationCup == $pickup2 || $destinationCup == $pickup3) {
        $destinationCup--;
        $destinationCup = $totalCups if $destinationCup == 0;
    }

    $cups[$pickup3] = $cups[$destinationCup];
    $cups[$destinationCup] = $pickup1;

    $currentCup = $cups[$currentCup];
}

my $cup1 = $cups[1];
my $cup2 = $cups[$cup1];
print $cup1 * $cup2 . "\n";
