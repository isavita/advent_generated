
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die $!;
my $input = do { local $/; <$fh> };
close($fh);

my %targetSue = (
    "children"    => 3,
    "cats"        => 7,
    "samoyeds"    => 2,
    "pomeranians" => 3,
    "akitas"      => 0,
    "vizslas"     => 0,
    "goldfish"    => 5,
    "trees"       => 3,
    "cars"        => 2,
    "perfumes"    => 1,
);

sub auntSue {
    my $input = shift;
    my @lines = split("\n", $input);

    foreach my $line (@lines) {
        my ($sueNum, $thing1, $amount1, $thing2, $amount2, $thing3, $amount3) = $line =~ /Sue (\d+): (\w+): (\d+), (\w+): (\d+), (\w+): (\d+)/;

        $thing1 =~ s/:$//;
        $thing2 =~ s/:$//;
        $thing3 =~ s/:$//;

        my %readingsMap = (
            $thing1 => $amount1,
            $thing2 => $amount2,
            $thing3 => $amount3,
        );

        my $allRulesMatched = 1;

        foreach my $check ("cats", "trees") {
            if (exists $readingsMap{$check}) {
                if ($readingsMap{$check} <= $targetSue{$check}) {
                    $allRulesMatched = 0;
                }
                delete $readingsMap{$check};
            }
        }

        foreach my $check ("pomeranians", "goldfish") {
            if (exists $readingsMap{$check}) {
                if ($readingsMap{$check} >= $targetSue{$check}) {
                    $allRulesMatched = 0;
                }
                delete $readingsMap{$check};
            }
        }

        foreach my $thing (keys %readingsMap) {
            if ($targetSue{$thing} != $readingsMap{$thing}) {
                $allRulesMatched = 0;
            }
        }

        if ($allRulesMatched) {
            return $sueNum;
        }
    }

    die "expect return from loop";
}

my $res = auntSue($input);
print "$res\n";
