
#!/usr/bin/perl
use strict;
use warnings;
use List::Util qw(sum);
use Data::Dumper;

my @initial_floors;
my @extra_items = (
    { is_chip => 0, material => 'elerium' },
    { is_chip => 1, material => 'elerium' },
    { is_chip => 0, material => 'dilithium' },
    { is_chip => 1, material => 'dilithium' }
);

# Read input file
open(my $fh, '<', 'input.txt') or die "Cannot open input.txt: $!";
my @lines = <$fh>;
close $fh;

# Parse input
for my $line_index (0..$#lines) {
    my @parts = split /\s+/, $lines[$line_index];
    
    for my $i (0..$#parts) {
        $parts[$i] =~ s/[,.]//g;
        
        if ($parts[$i] eq 'generator') {
            push @{$initial_floors[$line_index]}, { 
                is_chip => 0, 
                material => $parts[$i-1] 
            };
        }
        elsif ($parts[$i] eq 'microchip') {
            my $material = $parts[$i-1];
            $material =~ s/-compatible//;
            push @{$initial_floors[$line_index]}, { 
                is_chip => 1, 
                material => $material 
            };
        }
    }
}

# Add extra items to first floor
push @{$initial_floors[0]}, @extra_items;

# Main solving function
sub solve_rtg_hell_day {
    my ($initial_state) = @_;
    my %seen_states;
    my @queue = ($initial_state);

    while (@queue) {
        my $current = shift @queue;
        
        # Check if done
        if (is_done($current)) {
            return $current->{steps};
        }

        # Generate hash key for state
        my $state_key = generate_state_key($current);
        next if $seen_states{$state_key};
        $seen_states{$state_key} = 1;

        # Generate next possible states
        my @next_states = get_next_states($current);
        push @queue, @next_states;
    }

    return -1;
}

# Check if state is valid (no chips fried)
sub is_valid {
    my ($state) = @_;
    
    for my $floor (@{$state->{floors}}) {
        my %generators = map { $_->{material} => 1 } 
                         grep { !$_->{is_chip} } @$floor;
        
        next unless keys %generators;
        
        for my $item (@$floor) {
            return 0 if $item->{is_chip} && !$generators{$item->{material}};
        }
    }
    
    return 1;
}

# Check if all items are on top floor
sub is_done {
    my ($state) = @_;
    my $total_items = sum map { scalar @$_ } @{$state->{floors}}[0..2];
    return $total_items == 0;
}

# Generate unique state key
sub generate_state_key {
    my ($state) = @_;
    my %gen_locations;
    my %chip_locations;
    
    for my $floor (0..$#{$state->{floors}}) {
        for my $item (@{$state->{floors}[$floor]}) {
            if ($item->{is_chip}) {
                $chip_locations{$item->{material}} = $floor;
            } else {
                $gen_locations{$item->{material}} = $floor;
            }
        }
    }
    
    my @pairs = map { [$gen_locations{$_}, $chip_locations{$_}] } 
                sort keys %gen_locations;
    
    return join(',', $state->{elevator}, 
                map { join('-', @$_) } sort { $a->[0] <=> $b->[0] || $a->[1] <=> $b->[1] } @pairs);
}

# Generate next possible states
sub get_next_states {
    my ($state) = @_;
    my @next_states;
    
    my @elevator_moves = ($state->{elevator} > 0 ? -1 : (), 
                          $state->{elevator} < 3 ? 1 : ());
    
    for my $move (@elevator_moves) {
        my $new_level = $state->{elevator} + $move;
        
        # Try moving 1 or 2 items
        my $current_floor = $state->{floors}[$state->{elevator}];
        for my $i (0..$#$current_floor) {
            # Move single item
            my $new_state = clone_state($state);
            $new_state->{elevator} = $new_level;
            $new_state->{steps}++;
            
            my $item = splice(@{$new_state->{floors}[$state->{elevator}]}, $i, 1);
            push @{$new_state->{floors}[$new_level]}, $item;
            
            push @next_states, $new_state if is_valid($new_state);
            
            # Move two items
            for my $j ($i+1..$#$current_floor) {
                $new_state = clone_state($state);
                $new_state->{elevator} = $new_level;
                $new_state->{steps}++;
                
                my $item1 = splice(@{$new_state->{floors}[$state->{elevator}]}, $j, 1);
                my $item2 = splice(@{$new_state->{floors}[$state->{elevator}]}, $i, 1);
                push @{$new_state->{floors}[$new_level]}, $item1, $item2;
                
                push @next_states, $new_state if is_valid($new_state);
            }
        }
    }
    
    return @next_states;
}

# Deep clone state
sub clone_state {
    my ($state) = @_;
    my $clone = {
        elevator => $state->{elevator},
        steps => $state->{steps},
        floors => [
            map { [ map { { %$_ } } @$_ ] } @{$state->{floors}}
        ]
    };
    return $clone;
}

# Initial state setup
my $initial_state = {
    floors => \@initial_floors,
    elevator => 0,
    steps => 0
};

# Solve and print result
my $result = solve_rtg_hell_day($initial_state);
print "$result\n";
