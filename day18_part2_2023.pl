
use strict;
use warnings;

my $North = {X => 0, Y => -1};
my $West  = {X => -1, Y => 0};
my $South = {X => 0, Y => 1};
my $East  = {X => 1, Y => 0};

sub Abs {
    my $x = shift;
    if ($x < 0) {
        return -$x;
    } else {
        return $x;
    }
}

sub parseInput {
    my @input = @_;
    my $current = {X => 0, Y => 0};
    my @vertices = ($current);

    for my $line (@input) {
        my @parts = split(" ", $line);
        my $color = $parts[2];
        my $dirInput = substr($color, 7, 1);
        my $lengthStr = substr($color, 2, 5);
        my $length = hexStringToInt($lengthStr);

        my $dir;
        if ($dirInput eq '3') {
            $dir = $North;
        } elsif ($dirInput eq '2') {
            $dir = $West;
        } elsif ($dirInput eq '1') {
            $dir = $South;
        } elsif ($dirInput eq '0') {
            $dir = $East;
        }

        $current = {X => $current->{X} + $dir->{X} * $length, Y => $current->{Y} + $dir->{Y} * $length};
        push @vertices, $current;
    }

    return @vertices;
}

sub hexStringToInt {
    my $hexStr = shift;
    my $num = hex($hexStr);
    return $num;
}

sub shoelace {
    my @vertices = @_;
    my $n = scalar @vertices;
    my $area = 0;

    for my $i (0..$n-1) {
        my $next = ($i + 1) % $n;
        $area += $vertices[$i]{X} * $vertices[$next]{Y};
        $area -= $vertices[$i]{Y} * $vertices[$next]{X};
    }

    $area = Abs($area) / 2;
    return $area;
}

sub perimeter {
    my @vertices = @_;
    my $n = scalar @vertices;
    my $perim = 0;

    for my $i (0..$n-1) {
        my $next = ($i + 1) % $n;
        $perim += Abs($vertices[$i]{X} - $vertices[$next]{X}) + Abs($vertices[$i]{Y} - $vertices[$next]{Y});
    }

    return $perim;
}

sub calculatePolygonArea {
    my @vertices = @_;
    return shoelace(@vertices) + perimeter(@vertices) / 2 + 1;
}

sub solve {
    my @input = @_;
    my @vertices = parseInput(@input);
    my $res = calculatePolygonArea(@vertices);
    return $res;
}

sub readFile {
    my $fileName = shift;
    open(my $fh, '<', $fileName) or die "Cannot open file $fileName: $!";
    my @lines = <$fh>;
    close($fh);
    chomp @lines;
    return @lines;
}

my @input = readFile("input.txt");
print solve(@input);
