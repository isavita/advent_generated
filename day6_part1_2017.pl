
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die "Could not open file 'input.txt' $!";
my $data = do { local $/; <$fh> };
close($fh);

my @strArr = split(/\s+/, $data);
my @banks;
foreach my $num (@strArr) {
    my $n = int($num);
    push @banks, $n;
}

my %seen;
my $cycles = 0;

while (1) {
    my $state = join(",", @banks);

    if ($seen{$state}) {
        last;
    }
    $seen{$state} = 1;

    my $maxIndex = 0;
    for (my $i = 1; $i < scalar @banks; $i++) {
        if ($banks[$i] > $banks[$maxIndex]) {
            $maxIndex = $i;
        }
    }

    my $blocks = $banks[$maxIndex];
    $banks[$maxIndex] = 0;
    for (my $i = 1; $i <= $blocks; $i++) {
        $banks[($maxIndex+$i) % scalar @banks]++;
    }

    $cycles++;
}

print "It takes $cycles redistribution cycles to reach a repeated configuration.\n";
