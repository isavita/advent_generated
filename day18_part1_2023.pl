
use strict;
use warnings;

sub add {
    my ($c1, $c2) = @_;
    return [$c1->[0] + $c2->[0], $c1->[1] + $c2->[1]];
}

sub multiply_by_scalar {
    my ($c, $s) = @_;
    return [$c->[0] * $s, $c->[1] * $s];
}

sub abs {
    my ($x) = @_;
    return $x < 0 ? -$x : $x;
}

sub parse_input {
    my @input = @_;
    my $current = [0, 0];
    my @vertices = ($current);

    my %directions = (
        'U' => [0, -1],
        'L' => [-1, 0],
        'D' => [0, 1],
        'R' => [1, 0]
    );

    foreach my $line (@input) {
        my @parts = split(' ', $line);
        my $dir_input = substr($parts[0], 0, 1);
        my $length_str = $parts[1];
        my $length = 0;
        foreach my $char (split('', $length_str)) {
            $length = $length * 10 + $char;
        }

        my $dir = $directions{$dir_input};
        $current = add($current, multiply_by_scalar($dir, $length));
        push @vertices, $current;
    }

    return @vertices;
}

sub hex_string_to_int {
    my ($hex_str) = @_;
    my $num = hex($hex_str);
    return $num;
}

sub shoelace {
    my @vertices = @_;
    my $n = scalar @vertices;
    my $area = 0;

    for (my $i = 0; $i < $n; $i++) {
        my $next = ($i + 1) % $n;
        $area += $vertices[$i][0] * $vertices[$next][1];
        $area -= $vertices[$i][1] * $vertices[$next][0];
    }

    $area = abs($area) / 2;
    return $area;
}

sub perimeter {
    my @vertices = @_;
    my $n = scalar @vertices;
    my $perim = 0;

    for (my $i = 0; $i < $n; $i++) {
        my $next = ($i + 1) % $n;
        $perim += abs($vertices[$i][0] - $vertices[$next][0]) + abs($vertices[$i][1] - $vertices[$next][1]);
    }

    return $perim;
}

sub calculate_polygon_area {
    my @vertices = @_;
    return shoelace(@vertices) + perimeter(@vertices) / 2 + 1;
}

sub solve {
    my @input = @_;
    my @vertices = parse_input(@input);
    my $res = calculate_polygon_area(@vertices);
    return $res;
}

sub read_file {
    my ($file_name) = @_;
    open(my $fh, '<', $file_name) or die "Cannot open file: $!";
    my @lines = <$fh>;
    close($fh);
    chomp @lines;
    return @lines;
}

my @input = read_file("input.txt");
my $result = solve(@input);
print "$result\n";
