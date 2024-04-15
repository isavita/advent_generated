use strict;
use warnings;

sub read_file {
    my ($filename) = @_;
    open my $fh, '<', $filename or die "Cannot open file: $!";
    my @lines = map { chomp; $_ } <$fh>;
    close $fh;
    return @lines;
}

sub build_grid {
    my @input = @_;
    my %data;
    my $width = length $input[0];
    my $height = scalar @input;

    for my $y (0 .. $#input) {
        my @chars = split '', $input[$y];
        for my $x (0 .. $#chars) {
            $data{"$x,$y"} = $chars[$x] if $chars[$x] ne '.';
        }
    }

    return ($width, $height, \%data);
}

sub is_in_bounds {
    my ($x, $y, $width, $height) = @_;
    return 0 <= $x && $x < $width && 0 <= $y && $y < $height;
}

sub shift_single_rock {
    my ($data, $x, $y, $dx, $dy, $width, $height) = @_;
    my $current = "$x,$y";

    if ($data->{$current} && $data->{$current} eq 'O') {
        my $next_x = $x + $dx;
        my $next_y = $y + $dy;
        my $next = "$next_x,$next_y";

        while (!exists $data->{$next} && is_in_bounds($next_x, $next_y, $width, $height)) {
            $data->{$next} = 'O';
            delete $data->{$current};

            $current = $next;
            $next_x += $dx;
            $next_y += $dy;
            $next = "$next_x,$next_y";
        }
    }
}

sub shift_rocks {
    my ($data, $width, $height, $dx, $dy) = @_;
    if ($dx == 0 && $dy == -1) {  # North
        for my $x (0 .. $width-1) {
            for my $y (0 .. $height-1) {
                shift_single_rock($data, $x, $y, $dx, $dy, $width, $height);
            }
        }
    }
}

sub calculate_load {
    my ($data, $width, $height) = @_;
    my $load = 0;

    for my $x (0 .. $width-1) {
        for my $y (0 .. $height-1) {
            my $coord = "$x,$y";
            if (exists $data->{$coord} && $data->{$coord} eq 'O') {
                $load += $height - $y;
            }
        }
    }

    return $load;
}

sub solve {
    my @input = @_;
    my ($width, $height, $data) = build_grid(@input);
    shift_rocks($data, $width, $height, 0, -1);  # North
    return calculate_load($data, $width, $height);
}

my @input = read_file("input.txt");
print solve(@input), "\n";