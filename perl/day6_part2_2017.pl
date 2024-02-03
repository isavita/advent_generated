
use strict;
use warnings;

# Step 1: Read Input
open my $fh, '<', 'input.txt' or die "Cannot open file: $!";
my $data = do { local $/; <$fh> };
close $fh;

my @strArr = split /\s+/, $data;
my @banks;
foreach my $num (@strArr) {
    my $n = int($num);
    push @banks, $n;
}

# Step 2: Initialize Variables
my %seen;
my $cycles = 0;

# Step 3: Redistribution Loop
while (1) {
    # Convert current banks state to string to store in map
    my $state = join(',', @banks);

    # Step 4: Check for Repeats
    if (exists $seen{$state}) {
        print "The size of the loop is " . ($cycles - $seen{$state}) . "\n";
        last;
    }
    $seen{$state} = $cycles;

    # Find the bank with most blocks
    my $maxIndex = 0;
    for my $i (1 .. $#banks) {
        $maxIndex = $i if $banks[$i] > $banks[$maxIndex];
    }

    # Perform redistribution
    my $blocks = $banks[$maxIndex];
    $banks[$maxIndex] = 0;
    for my $i (1 .. $blocks) {
        $banks[($maxIndex + $i) % @banks]++;
    }

    # Increment cycle counter
    $cycles++;
}
