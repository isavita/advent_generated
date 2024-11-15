
#!/usr/bin/perl
use strict;
use warnings;
use Data::Dumper;

# Read input tiles
my %tiles;
my $current_tile;

open(my $fh, '<', 'input.txt') or die "Cannot open input.txt: $!";
while (my $line = <$fh>) {
    chomp $line;
    
    if ($line =~ /Tile (\d+):/) {
        $current_tile = $1;
        $tiles{$current_tile} = [];
    } elsif ($line) {
        push @{$tiles{$current_tile}}, $line;
    }
}
close $fh;

# Get all possible borders for each tile
my %tile_borders;
foreach my $tile_id (keys %tiles) {
    $tile_borders{$tile_id} = get_borders($tiles{$tile_id});
}

# Find corner tiles
my @corner_tiles;
foreach my $tile_id (keys %tiles) {
    my $matching_borders = 0;
    
    foreach my $border (@{$tile_borders{$tile_id}}) {
        my $matches = 0;
        foreach my $other_tile_id (keys %tiles) {
            next if $other_tile_id eq $tile_id;
            
            foreach my $other_border (@{$tile_borders{$other_tile_id}}) {
                $matches++ if $border eq $other_border || $border eq reverse($other_border);
            }
        }
        $matching_borders++ if $matches > 0;
    }
    
    push @corner_tiles, $tile_id if $matching_borders == 2;
}

# Calculate result
my $result = 1;
$result *= $_ for @corner_tiles;
print "Corner tiles multiplied: $result\n";

# Get all borders for a tile
sub get_borders {
    my ($tile) = @_;
    my @borders;
    
    # Top border
    push @borders, $tile->[0];
    
    # Bottom border
    push @borders, $tile->[-1];
    
    # Left border
    push @borders, join('', map { substr($_, 0, 1) } @$tile);
    
    # Right border
    push @borders, join('', map { substr($_, -1) } @$tile);
    
    return \@borders;
}
