use strict;
use warnings;

my @reboot_steps;

open my $file, '<', 'input.txt' or die "Could not open file 'input.txt': $!";
while (my $line = <$file>) {
    chomp $line;
    next if $line eq '';
    push @reboot_steps, parse_reboot_step($line);
}
close $file;

my ($min_coord, $max_coord) = (-50, 50);
my @cube_grid = create_cube_grid($min_coord, $max_coord);
execute_reboot_steps(\@cube_grid, \@reboot_steps);
my $on_cubes = count_on_cubes(\@cube_grid);

print "$on_cubes\n";

sub parse_reboot_step {
    my ($line) = @_;
    my ($action, $ranges) = split ' ', $line;
    my ($x_range, $y_range, $z_range) = split ',', $ranges;
    my ($x_start, $x_end) = $x_range =~ /x=(-?\d+)\.\.(-?\d+)/;
    my ($y_start, $y_end) = $y_range =~ /y=(-?\d+)\.\.(-?\d+)/;
    my ($z_start, $z_end) = $z_range =~ /z=(-?\d+)\.\.(-?\d+)/;
    return {
        action => $action,
        x_start => $x_start,
        x_end => $x_end,
        y_start => $y_start,
        y_end => $y_end,
        z_start => $z_start,
        z_end => $z_end
    };
}

sub create_cube_grid {
    my ($min_coord, $max_coord) = @_;
    my $grid_size = $max_coord - $min_coord + 1;
    my @grid;
    for my $i (0 .. $grid_size - 1) {
        for my $j (0 .. $grid_size - 1) {
            for my $k (0 .. $grid_size - 1) {
                $grid[$i][$j][$k] = 0;
            }
        }
    }
    return @grid;
}

sub execute_reboot_steps {
    my ($cube_grid, $reboot_steps) = @_;
    for my $step (@$reboot_steps) {
        next unless $step->{x_start} >= -50 && $step->{x_end} <= 50 &&
                    $step->{y_start} >= -50 && $step->{y_end} <= 50 &&
                    $step->{z_start} >= -50 && $step->{z_end} <= 50;
        for my $x ($step->{x_start} .. $step->{x_end}) {
            for my $y ($step->{y_start} .. $step->{y_end}) {
                for my $z ($step->{z_start} .. $step->{z_end}) {
                    $cube_grid->[$x+50][$y+50][$z+50] = $step->{action} eq 'on' ? 1 : 0;
                }
            }
        }
    }
}

sub count_on_cubes {
    my ($cube_grid) = @_;
    my $count = 0;
    for my $i (0 .. $#{$cube_grid}) {
        for my $j (0 .. $#{$cube_grid->[$i]}) {
            for my $k (0 .. $#{$cube_grid->[$i][$j]}) {
                $count++ if $cube_grid->[$i][$j][$k];
            }
        }
    }
    return $count;
}