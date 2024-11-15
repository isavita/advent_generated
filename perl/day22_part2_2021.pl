
#!/usr/bin/perl
use strict;
use warnings;
use List::Util qw(max min);

sub parse_input {
    my ($input) = @_;
    my @cubes;
    for my $line (split /\n/, $input) {
        my ($state, $coords) = split / /, $line;
        my ($x1, $x2, $y1, $y2, $z1, $z2);
        if ($coords =~ /x=(-?\d+)\.\.(-?\d+),y=(-?\d+)\.\.(-?\d+),z=(-?\d+)\.\.(-?\d+)/) {
            ($x1, $x2, $y1, $y2, $z1, $z2) = ($1, $2, $3, $4, $5, $6);
        }
        
        push @cubes, {
            is_on => $state eq 'on',
            x1 => $x1, x2 => $x2,
            y1 => $y1, y2 => $y2,
            z1 => $z1, z2 => $z2
        };
    }
    return @cubes;
}

sub get_intersection {
    my ($c1, $c2) = @_;
    my $x1 = max($c1->{x1}, $c2->{x1});
    my $x2 = min($c1->{x2}, $c2->{x2});
    my $y1 = max($c1->{y1}, $c2->{y1});
    my $y2 = min($c1->{y2}, $c2->{y2});
    my $z1 = max($c1->{z1}, $c2->{z1});
    my $z2 = min($c1->{z2}, $c2->{z2});

    return undef if $x1 > $x2 || $y1 > $y2 || $z1 > $z2;

    my $intersection_state = 
        ($c1->{is_on} && $c2->{is_on}) ? 0 :
        (!$c1->{is_on} && !$c2->{is_on}) ? 1 :
        $c2->{is_on};

    return {
        is_on => $intersection_state,
        x1 => $x1, x2 => $x2,
        y1 => $y1, y2 => $y2,
        z1 => $z1, z2 => $z2
    };
}

sub volume {
    my ($cube) = @_;
    my $vol = ($cube->{x2} - $cube->{x1} + 1) * 
              ($cube->{y2} - $cube->{y1} + 1) * 
              ($cube->{z2} - $cube->{z1} + 1);
    return $cube->{is_on} ? $vol : -$vol;
}

sub solve {
    my ($input) = @_;
    my @cubes = parse_input($input);
    my @final_list;

    for my $cube (@cubes) {
        my @to_add;
        
        for my $final_cube (@final_list) {
            my $intersection = get_intersection($final_cube, $cube);
            push @to_add, $intersection if $intersection;
        }

        push @to_add, $cube if $cube->{is_on};
        push @final_list, @to_add;
    }

    my $total = 0;
    $total += volume($_) for @final_list;
    
    return $total;
}

open my $fh, '<', 'input.txt' or die "Cannot open input.txt: $!";
my $input = do { local $/; <$fh> };
close $fh;

chomp $input;
print solve($input), "\n";
