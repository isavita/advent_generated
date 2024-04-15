use strict;
use warnings;

use constant {
    Wall => '#',
    Free => '.',
};

sub is_letter {
    my ($c) = @_;
    return $c ge 'A' && $c le 'Z';
}

sub neighbours {
    my ($x, $y) = @_;
    return (
        [$x, $y + 1],
        [$x + 1, $y],
        [$x, $y - 1],
        [$x - 1, $y],
    );
}

sub parse {
    my %grid;
    my ($x_max, $y_max) = (0, 0);

    open my $file, '<', 'input.txt' or die "Cannot open file: $!";

    my $i = 0;
    while (my $line = <$file>) {
        chomp $line;

        $y_max = length($line) if length($line) > $y_max;

        for my $j (0 .. length($line) - 1) {
            $grid{$i}{$j} = substr($line, $j, 1);
        }

        $i++;
    }
    $x_max = $i;

    my ($aa, $zz);
    my %is_outer;
    my %portal_name;
    my %teleport;

    my %cache;

    for my $i (0 .. $x_max - 1) {
        for my $j (0 .. $y_max - 1) {
            my $c = $grid{$i}{$j};

            next unless is_letter($c);

            my ($p_name, $p_point, $ok) = extract_portal(\%grid, $i, $j);

            next unless $ok;

            $portal_name{$p_point->[0]}{$p_point->[1]} = $p_name;

            if ($p_name eq 'AA') {
                $aa = $p_point;
                $is_outer{$p_point->[0]}{$p_point->[1]} = 1;
                next;
            }

            if ($p_name eq 'ZZ') {
                $zz = $p_point;
                $is_outer{$p_point->[0]}{$p_point->[1]} = 1;
                next;
            }

            if (exists $cache{$p_name}) {
                my $target = $cache{$p_name};
                $teleport{$p_point->[0]}{$p_point->[1]} = $target;
                $teleport{$target->[0]}{$target->[1]} = $p_point;
            } else {
                $cache{$p_name} = $p_point;
            }

            if ($j == 0 || $i == 0 || $i == $x_max - 2 || $j == $y_max - 2) {
                $is_outer{$p_point->[0]}{$p_point->[1]} = 1;
            } else {
                $is_outer{$p_point->[0]}{$p_point->[1]} = 0;
            }
        }
    }

    return {
        x_max       => $x_max,
        y_max       => $y_max,
        grid        => \%grid,
        aa          => $aa,
        zz          => $zz,
        teleport    => \%teleport,
        portal_name => \%portal_name,
        is_outer    => \%is_outer,
    };
}

sub extract_portal {
    my ($grid, $x, $y) = @_;
    my $c1 = $grid->{$x}{$y};

    if (exists $grid->{$x + 1}{$y} && is_letter($grid->{$x + 1}{$y})) {
        my $c2 = $grid->{$x + 1}{$y};
        my $portal_name = $c1 . $c2;

        my $portal_point = [$x + 2, $y];
        return ($portal_name, $portal_point, 1) if $grid->{$portal_point->[0]}{$portal_point->[1]} eq '.';

        $portal_point = [$x - 1, $y];
        return ($portal_name, $portal_point, 1) if $grid->{$portal_point->[0]}{$portal_point->[1]} eq '.';
    }

    if (exists $grid->{$x}{$y + 1} && is_letter($grid->{$x}{$y + 1})) {
        my $c2 = $grid->{$x}{$y + 1};
        my $portal_name = $c1 . $c2;

        my $portal_point = [$x, $y + 2];
        return ($portal_name, $portal_point, 1) if $grid->{$portal_point->[0]}{$portal_point->[1]} eq '.';

        $portal_point = [$x, $y - 1];
        return ($portal_name, $portal_point, 1) if $grid->{$portal_point->[0]}{$portal_point->[1]} eq '.';
    }

    return ('', [], 0);
}

sub bfs_nested {
    my ($map) = @_;

    my %discovered;
    my @to_do;

    my $root = [$map->{aa}->[0], $map->{aa}->[1], 0];

    $discovered{$root->[0]}{$root->[1]}{$root->[2]} = 1;
    push @to_do, $root;

    my $steps = 0;

    while (@to_do) {
        my $level_size = scalar @to_do;

        while ($level_size > 0) {
            my $curr = shift @to_do;
            $level_size--;

            for my $n (neighbours($curr->[0], $curr->[1])) {
                my $dest = $map->{grid}{$n->[0]}{$n->[1]};

                if ($dest eq Wall) {
                    next;
                } elsif ($dest eq Free) {
                    my $target = [$n->[0], $n->[1], $curr->[2]];

                    unless (exists $discovered{$target->[0]}{$target->[1]}{$target->[2]}) {
                        $discovered{$target->[0]}{$target->[1]}{$target->[2]} = 1;
                        push @to_do, $target;
                    }
                } elsif (is_letter($dest)) {
                    my $is_outer = $map->{is_outer}{$curr->[0]}{$curr->[1]};

                    my $target;
                    if (!$is_outer) {
                        $target = [
                            $map->{teleport}{$curr->[0]}{$curr->[1]}[0],
                            $map->{teleport}{$curr->[0]}{$curr->[1]}[1],
                            $curr->[2] + 1,
                        ];
                    } else {
                        my $portal_name = $map->{portal_name}{$curr->[0]}{$curr->[1]};
                        if ($curr->[2] == 0) {
                            return $steps if $portal_name eq 'ZZ';
                            next;
                        }

                        next if $portal_name eq 'AA' || $portal_name eq 'ZZ';

                        $target = [
                            $map->{teleport}{$curr->[0]}{$curr->[1]}[0],
                            $map->{teleport}{$curr->[0]}{$curr->[1]}[1],
                            $curr->[2] - 1,
                        ];
                    }

                    unless (exists $discovered{$target->[0]}{$target->[1]}{$target->[2]}) {
                        $discovered{$target->[0]}{$target->[1]}{$target->[2]} = 1;
                        push @to_do, $target;
                    }
                }
            }
        }

        $steps++;
    }

    return -1;
}

my $map = parse();
my $result = bfs_nested($map);
print "$result\n";