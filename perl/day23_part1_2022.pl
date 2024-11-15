
#!/usr/bin/perl
use strict;
use warnings;
use List::Util qw(min max);

# Read input from file
open(my $fh, '<', 'input.txt') or die "Cannot open file: $!";
my @grid = <$fh>;
chomp @grid;
close $fh;

# Directions
my @directions = (
    {name => 'N', check => [[-1,-1],[-1,0],[-1,1]], move => [-1,0]},
    {name => 'S', check => [[1,-1],[1,0],[1,1]], move => [1,0]},
    {name => 'W', check => [[-1,-1],[0,-1],[1,-1]], move => [0,-1]},
    {name => 'E', check => [[-1,1],[0,1],[1,1]], move => [0,1]}
);

# Convert grid to hash of elf positions
my %elves;
for my $r (0..$#grid) {
    for my $c (0..length($grid[$r])-1) {
        $elves{"$r,$c"} = 1 if substr($grid[$r], $c, 1) eq '#';
    }
}

# Simulate rounds
for my $round (1..10) {
    my %proposals;
    my %proposal_count;

    # Propose moves
    for my $elf (keys %elves) {
        my ($r, $c) = split(',', $elf);
        my $has_neighbor = 0;
        
        # Check all 8 adjacent positions
        for my $dr (-1..1) {
            for my $dc (-1..1) {
                next if $dr == 0 && $dc == 0;
                $has_neighbor = 1 if exists $elves{($r+$dr).",".($c+$dc)};
            }
        }

        # If no neighbors, skip
        next unless $has_neighbor;

        # Try directions
        for my $dir (@directions) {
            my $valid = 1;
            for my $check (@{$dir->{check}}) {
                my $nr = $r + $check->[0];
                my $nc = $c + $check->[1];
                if (exists $elves{"$nr,$nc"}) {
                    $valid = 0;
                    last;
                }
            }

            if ($valid) {
                my $nr = $r + $dir->{move}[0];
                my $nc = $c + $dir->{move}[1];
                my $proposal = "$nr,$nc";
                $proposals{$elf} = $proposal;
                $proposal_count{$proposal}++;
                last;
            }
        }
    }

    # Move elves
    my %new_elves;
    for my $elf (keys %elves) {
        if (exists $proposals{$elf} && $proposal_count{$proposals{$elf}} == 1) {
            $new_elves{$proposals{$elf}} = 1;
        } else {
            $new_elves{$elf} = 1;
        }
    }
    %elves = %new_elves;

    # Rotate directions
    push @directions, shift @directions;
}

# Calculate empty ground tiles
my @elf_rows = map { (split(','))[0] } keys %elves;
my @elf_cols = map { (split(','))[1] } keys %elves;
my $min_row = min(@elf_rows);
my $max_row = max(@elf_rows);
my $min_col = min(@elf_cols);
my $max_col = max(@elf_cols);

my $empty_tiles = 0;
for my $r ($min_row..$max_row) {
    for my $c ($min_col..$max_col) {
        $empty_tiles++ unless exists $elves{"$r,$c"};
    }
}

print "Empty ground tiles: $empty_tiles\n";
