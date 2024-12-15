
#!/usr/bin/perl
use strict;
use warnings;

sub mod {
    my ($a, $b) = @_;
    return ($a % $b + $b) % $b;
}

sub parse_line {
    my ($line) = @_;
    if ($line =~ /p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)/) {
        return { x => $1, y => $2, vx => $3, vy => $4 };
    } else {
        die "Invalid line format: $line";
    }
}

sub move_robots {
    my ($robots, $size_x, $size_y) = @_;
    foreach my $robot (@$robots) {
        $robot->{x} = mod($robot->{x} + $robot->{vx}, $size_x);
        $robot->{y} = mod($robot->{y} + $robot->{vy}, $size_y);
    }
}

sub count_quadrants {
    my ($robots, $size_x, $size_y) = @_;
    my @counts = (0, 0, 0, 0);
    my $center_x = int($size_x / 2);
    my $center_y = int($size_y / 2);

    foreach my $robot (@$robots) {
        my ($x, $y) = ($robot->{x}, $robot->{y});
        if ($x < $center_x) {
            if ($y < $center_y) {
                $counts[0]++;
            } elsif ($y > $center_y) {
                $counts[1]++;
            }
        } elsif ($x > $center_x) {
            if ($y < $center_y) {
                $counts[2]++;
            } elsif ($y > $center_y) {
                $counts[3]++;
            }
        }
    }
    return @counts;
}

sub has_no_overlaps {
    my ($robots) = @_;
    my %positions;
    foreach my $robot (@$robots) {
        my $pos = join ',', $robot->{x}, $robot->{y};
        return 0 if $positions{$pos}++;
    }
    return 1;
}

sub draw_grid {
    my ($robots, $size_x, $size_y) = @_;
    my %grid_map;
    foreach my $robot (@$robots) {
        $grid_map{join ',', $robot->{x}, $robot->{y}} = 1;
    }

    for (my $y = 0; $y < $size_y; $y++) {
        my $line = "";
        for (my $x = 0; $x < $size_x; $x++) {
            $line .= $grid_map{join ',', $x, $y} ? "#" : ".";
        }
        print "$line\n";
    }
}

my $size_x = 101;
my $size_y = 103;

open my $fh, "<", "input.txt" or die "Could not open input.txt: $!";
my @robots;
while (my $line = <$fh>) {
    chomp $line;
    next if $line eq "";
    push @robots, parse_line($line);
}
close $fh;

my @robots_part1 = map { { %$_ } } @robots;
for (my $n = 0; $n < 100; $n++) {
    move_robots(\@robots_part1, $size_x, $size_y);
}

my @counts = count_quadrants(\@robots_part1, $size_x, $size_y);
my $safety_factor = 1;
$safety_factor *= $_ for @counts;
print "Part 1 - Safety Factor after 100 seconds: $safety_factor\n";

my @robots_part2 = map { { %$_ } } @robots;
my $seconds = 0;
while (1) {
    last if has_no_overlaps(\@robots_part2);
    move_robots(\@robots_part2, $size_x, $size_y);
    $seconds++;
    if ($seconds > 1000000) {
        die "Exceeded maximum iterations without finding a unique position configuration.";
    }
}
print "Part 2 - Fewest seconds to display Easter egg: $seconds\n";
print "Final positions of robots:\n";
draw_grid(\@robots_part2, $size_x, $size_y);
