
#!/usr/bin/perl
use strict;
use warnings;

# Read input from file
open(my $fh, '<', 'input.txt') or die "Cannot open file: $!";

# Read times and distances
my $time_line = <$fh>;
my $distance_line = <$fh>;

# Extract times and distances
my @times = $time_line =~ /\d+/g;
my @distances = $distance_line =~ /\d+/g;

# Calculate ways to win for each race
my $total_ways = 1;
for my $i (0..$#times) {
    my $race_time = $times[$i];
    my $record_distance = $distances[$i];
    
    my $ways_to_win = count_winning_ways($race_time, $record_distance);
    $total_ways *= $ways_to_win;
}

# Print result
print "Total ways to win: $total_ways\n";

# Function to count ways to win a race
sub count_winning_ways {
    my ($race_time, $record_distance) = @_;
    my $ways = 0;
    
    for my $hold_time (1..$race_time-1) {
        my $travel_time = $race_time - $hold_time;
        my $distance = $hold_time * $travel_time;
        
        $ways++ if $distance > $record_distance;
    }
    
    return $ways;
}

close($fh);
