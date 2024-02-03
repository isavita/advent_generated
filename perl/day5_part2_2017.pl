
use strict;
use warnings;

# Step 1: Read Input
open(my $fh, '<', 'input.txt') or die "Cannot open file: $!";
my @lines = <$fh>;
close($fh);

my @offsets;
foreach my $line (@lines) {
    my $offset = int($line);
    push @offsets, $offset;
}

# Step 2: Initialize Variables
my $index = 0;
my $steps = 0;

# Step 3: Navigate Maze
while ($index >= 0 && $index < scalar(@offsets)) {
    # Fetch the jump offset at the current index
    my $jump = $offsets[$index];

    # Step 4: Update Offset
    if ($jump >= 3) {
        $offsets[$index]--;
    } else {
        $offsets[$index]++;
    }

    # Move to the new index
    $index += $jump;

    # Increment steps counter
    $steps++;
}

# Step 6: Output
print "$steps\n";
