
use strict;
use warnings;
use Data::Dumper;
use feature 'say';

my @grid;
open(my $fh, '<', 'input.txt') or die "Could not open file 'input.txt' $!";
while (my $line = <$fh>) {
    chomp $line;
    push @grid, [split //, $line];
}
close $fh;

my $rows = scalar @grid;
my $cols = scalar @{$grid[0]};

my %dir_map = (
    0 => [0, 1],   # right
    1 => [1, 0],   # down
    2 => [0, -1],  # left
    3 => [-1, 0],  # up
);

sub solve {
    my %visited;
    my @queue = ([0, 0, 0, -1, 0]); # row, col, heat_loss, dir, steps
    
    while (@queue) {
        my ($r, $c, $heat_loss, $dir, $steps) = @{shift @queue};
        
        if ($r == $rows - 1 && $c == $cols - 1) {
            return $heat_loss;
        }
        
        my $key = "$r,$c,$dir,$steps";
        next if exists $visited{$key};
        $visited{$key} = 1;
        
        
        for my $new_dir (0..3) {
          next if $dir != -1 && abs($new_dir - $dir) == 2;
          
          my $new_steps = ($new_dir == $dir) ? $steps + 1 : 1;
          next if $new_steps > 3;
          
          my ($dr, $dc) = @{$dir_map{$new_dir}};
          my $new_r = $r + $dr;
          my $new_c = $c + $dc;
          
          next if $new_r < 0 || $new_r >= $rows || $new_c < 0 || $new_c >= $cols;
          
          my $new_heat_loss = $heat_loss + $grid[$new_r][$new_c];
          
          push @queue, [$new_r, $new_c, $new_heat_loss, $new_dir, $new_steps];
          @queue = sort { $a->[2] <=> $b->[2] } @queue;
        }
    }
    
    return -1; # Should not reach here
}

my $result = solve();
say $result;

