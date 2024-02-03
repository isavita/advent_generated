
use strict;
use warnings;

use List::Util qw(sum);

use constant {
    OPEN       => '.',
    TREES      => '|',
    LUMBERYARD => '#',
    SIZE       => 50,
};

my $grid = read_input("input.txt");

for (my $minute = 0; $minute < 10; $minute++) {
    $grid = transform($grid);
}

my ($wooded, $lumberyards) = count_resources($grid);
print $wooded * $lumberyards . "\n";

sub read_input {
    my ($filename) = @_;
    open my $fh, '<', $filename or die "Error reading file: $!";
    my @grid;
    while (my $line = <$fh>) {
        chomp $line;
        push @grid, [split //, $line];
    }
    close $fh;
    return \@grid;
}

sub transform {
    my ($grid) = @_;
    my @new_grid;
    for my $i (0 .. $#{$grid}) {
        for my $j (0 .. $#{$grid->[$i]}) {
            $new_grid[$i][$j] = next_acre_state($grid, $i, $j);
        }
    }
    return \@new_grid;
}

sub next_acre_state {
    my ($grid, $i, $j) = @_;
    my $acre = $grid->[$i][$j];
    if ($acre eq OPEN) {
        return TREES if count_adjacent($grid, $i, $j, TREES) >= 3;
    } elsif ($acre eq TREES) {
        return LUMBERYARD if count_adjacent($grid, $i, $j, LUMBERYARD) >= 3;
    } elsif ($acre eq LUMBERYARD) {
        return LUMBERYARD if count_adjacent($grid, $i, $j, LUMBERYARD) >= 1 && count_adjacent($grid, $i, $j, TREES) >= 1;
        return OPEN;
    }
    return $acre;
}

sub count_adjacent {
    my ($grid, $i, $j, $acre_type) = @_;
    my $count = 0;
    for my $x (-1, 0, 1) {
        for my $y (-1, 0, 1) {
            next if $x == 0 && $y == 0;
            if ($i + $x >= 0 && $i + $x < @{$grid} && $j + $y >= 0 && $j + $y < @{$grid->[$i]} && $grid->[$i + $x][$j + $y] eq $acre_type) {
                $count++;
            }
        }
    }
    return $count;
}

sub count_resources {
    my ($grid) = @_;
    my ($wooded, $lumberyards) = (0, 0);
    for my $i (0 .. $#{$grid}) {
        for my $j (0 .. $#{$grid->[$i]}) {
            $wooded++ if $grid->[$i][$j] eq TREES;
            $lumberyards++ if $grid->[$i][$j] eq LUMBERYARD;
        }
    }
    return ($wooded, $lumberyards);
}
