
use strict;
use warnings;

# Open the input file
open(my $fh, '<', 'input.txt') or die "Could not open file 'input.txt' $!";

my $total_points = 0;

while (my $line = <$fh>) {
    chomp $line;
    
    # Skip empty lines
    next if $line =~ /^\s*$/;

    # Split the line into winning numbers and your numbers
    my ($winning_numbers, $your_numbers) = split(/\s*\|\s*/, $line);
    
    # Convert the winning numbers and your numbers into arrays
    my @winning = split(/\s+/, $winning_numbers);
    my @your = split(/\s+/, $your_numbers);
    
    # Create a hash for quick lookup of winning numbers
    my %winning_hash = map { $_ => 1 } @winning;
    
    # Count matches
    my $matches = 0;
    foreach my $num (@your) {
        if (exists $winning_hash{$num}) {
            $matches++;
        }
    }
    
    # Calculate points for the card
    if ($matches > 0) {
        my $points = 1 << ($matches - 1); # 2^(matches - 1)
        $total_points += $points;
    }
}

# Close the file
close($fh);

# Print the total points
print "Total points: $total_points\n";
