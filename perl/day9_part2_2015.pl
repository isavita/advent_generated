
use strict;
use warnings;

my %distances = read_and_parse_input("input.txt");
my @locations = get_unique_locations(\%distances);
my $max_distance = find_longest_route(\@locations, \%distances);
print "$max_distance\n";

sub read_and_parse_input {
    my ($filename) = @_;
    open my $fh, '<', $filename or die "Error reading input: $!";
    
    my %distances;
    while (my $line = <$fh>) {
        my @parts = split(' ', $line);
        next if @parts != 5;
        
        my ($from, $to, $dist) = ($parts[0], $parts[2], $parts[4]);
        my $distance = int($dist);
        
        $distances{$from}->{$to} = $distance;
        $distances{$to}->{$from} = $distance; # Assuming distance is symmetric
    }
    
    close $fh;
    return %distances;
}

sub get_unique_locations {
    my ($distances) = @_;
    my %location_set;
    
    foreach my $from (keys %$distances) {
        $location_set{$from} = 1;
        foreach my $to (keys %{$distances->{$from}}) {
            $location_set{$to} = 1;
        }
    }
    
    return keys %location_set;
}

sub find_longest_route {
    my ($locations, $distances) = @_;
    my $max_distance = 0;
    permute($locations, 0, \$max_distance, $distances, 0);
    return $max_distance;
}

sub permute {
    my ($arr, $i, $best_distance, $distances, $find_shortest) = @_;
    
    if ($i > scalar(@$arr)) {
        return;
    }
    if ($i == scalar(@$arr)) {
        my $dist = calculate_route_distance($arr, $distances);
        if ($find_shortest) {
            if ($$best_distance == 0 || $dist < $$best_distance) {
                $$best_distance = $dist;
            }
        } else {
            if ($dist > $$best_distance) {
                $$best_distance = $dist;
            }
        }
        return;
    }
    for my $j ($i .. $#{$arr}) {
        @$arr[$i, $j] = @$arr[$j, $i];
        permute($arr, $i+1, $best_distance, $distances, $find_shortest);
        @$arr[$i, $j] = @$arr[$j, $i];
    }
}

sub calculate_route_distance {
    my ($route, $distances) = @_;
    my $sum = 0;
    for my $i (0 .. $#{$route}-1) {
        $sum += $distances->{$route->[$i]}->{$route->[$i+1]};
    }
    return $sum;
}
