
use strict;
use warnings;

# Step 1: Read Input
open my $fh, '<', 'input.txt' or die "Cannot open file: $!";
my @lines = <$fh>;
close $fh;

# Step 2: Create Data Structure
my %holderMap;
my %heldMap;

# Regular expression to match program names
my $re = qr/[a-z]+/;

foreach my $line (@lines) {
    my @names = $line =~ /$re/g;
    my $holder = $names[0];
    $holderMap{$holder} = 1;

    # If the program holds others, update the held map
    if (@names > 1) {
        foreach my $name (@names[1..$#names]) {
            $heldMap{$name} = 1;
        }
    }
}

# Step 3: Find Root
foreach my $holder (keys %holderMap) {
    if (!$heldMap{$holder}) {
        print "$holder\n";
        exit;
    }
}
