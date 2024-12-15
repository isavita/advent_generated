
#!/usr/bin/perl
use strict;
use warnings;

my %map;
my $size;
my @movements;
my @dirs = ([-1, 0], [0, 1], [1, 0], [0, -1]);

sub parse {
    open my $fh, '<', 'input.txt' or die "Could not open input.txt: $!";
    my $r = 0;
    while (my $line = <$fh>) {
        chomp $line;
        if ($line eq "") {
            last;
        }
        if ($r == 0) {
            $size = length($line) / 3;
        }
        for (my $c = 0; $c < length($line); $c++) {
            my $char = substr($line, $c, 1);
            next if $char eq ' ';
            $map{"$r,$c"} = ($char eq '#');
        }
        $r++;
    }
    my $path = <$fh>;
    chomp $path;
    @movements = parse_path($path);
    close $fh;
}

sub parse_path {
    my ($path) = @_;
    my @movements;
    my $acc = 0;
    for my $char (split //, $path) {
        if ($char eq 'R' || $char eq 'L') {
            push @movements, { steps => $acc } if $acc > 0;
            $acc = 0;
            push @movements, { rotate => $char };
        } else {
            $acc = 10 * $acc + ($char - '0');
        }
    }
    push @movements, { steps => $acc } if $acc > 0;
    return @movements;
}

sub rotate {
    my ($dir, $direction) = @_;
    return ($direction eq 'R') ? ($dir + 1) % 4 : ($dir - 1 + 4) % 4;
}

sub points {
    my ($dir) = @_;
    return ($dir + 3) % 4;
}

sub walk {
    my ($curr, $facing) = @_;
    my ($dx, $dy) = @{$dirs[$facing]};
    my $next_x = $curr->[0] + $dx;
    my $next_y = $curr->[1] + $dy;
    if (exists $map{"$next_x,$next_y"}) {
        return undef if $map{"$next_x,$next_y"};
        return [$next_x, $next_y], $facing;
    }
    my ($new_next, $new_facing) = cross_border([$next_x, $next_y], $facing);
    return undef if $map{"$new_next->[0],$new_next->[1]"};
    return $new_next, $new_facing;
}

sub cross_border {
    my ($n, $dir) = @_;
    my ($x, $y) = @$n;
    my $new_x;
    my $new_y;
    my $new_dir;

    if ($x == -1 && $y < 2 * $size) {
        $new_x = $y + 2 * $size;
        $new_y = $x + 1;
        $new_dir = 1;
    } elsif ($x == -1 && $y >= 2 * $size) {
        $new_x = $x + 4 * $size;
        $new_y = $y - 2 * $size;
        $new_dir = 0;
    } elsif ($x == $size && $dir == 2) {
        $new_x = $y - $size;
        $new_y = $x + $size - 1;
        $new_dir = 3;
    } elsif ($x == 2 * $size - 1 && $dir == 0) {
        $new_x = $y + $size;
        $new_y = $x - $size + 1;
        $new_dir = 1;
    } elsif ($x == 3 * $size && $dir == 2) {
        $new_x = $y + 2 * $size;
        $new_y = $x - 2 * $size - 1;
        $new_dir = 3;
    } elsif ($x == 4 * $size) {
        $new_x = $x - 4 * $size;
        $new_y = $y + 2 * $size;
        $new_dir = 2;
    } elsif ($y == -1 && $x < 3 * $size) {
        $new_x = 3 * $size - 1 - $x;
        $new_y = $y + $size + 1;
        $new_dir = 1;
    } elsif ($y == -1 && $x >= 3 * $size) {
        $new_x = $y + 1;
        $new_y = $x - 2 * $size;
        $new_dir = 2;
    } elsif ($y == $size - 1 && $x < $size) {
        $new_x = 3 * $size - 1 - $x;
        $new_y = $y - $size + 1;
        $new_dir = 1;
    } elsif ($y == $size - 1 && $x >= $size && $dir == 3) {
        $new_x = $y + $size + 1;
        $new_y = $x - $size;
        $new_dir = 2;
    } elsif ($y == $size && $dir == 1) {
        $new_x = $y + 2 * $size - 1;
        $new_y = $x - 2 * $size;
        $new_dir = 0;
    } elsif ($y == 2 * $size && $x < 2 * $size && $dir == 1) {
        $new_x = $y - $size - 1;
        $new_y = $x + $size;
        $new_dir = 0;
    } elsif ($y == 2 * $size && $x >= 2 * $size) {
        $new_x = 3 * $size - 1 - $x;
        $new_y = $y + $size - 1;
        $new_dir = 3;
    } elsif ($y == 3 * $size) {
        $new_x = 3 * $size - 1 - $x;
        $new_y = $y - $size - 1;
        $new_dir = 3;
    } else {
        die "not a border crossing";
    }
    return [$new_x, $new_y], $new_dir;
}

parse();

my $curr = [0, $size];
my $facing = 1;

for my $mov (@movements) {
    if (exists $mov->{rotate}) {
        $facing = rotate($facing, $mov->{rotate});
    } else {
        for (my $i = 0; $i < $mov->{steps}; $i++) {
            my ($next, $new_facing) = walk($curr, $facing);
            last unless defined $next;
            $curr = $next;
            $facing = $new_facing;
        }
    }
}

print 1000 * ($curr->[0] + 1) + 4 * ($curr->[1] + 1) + points($facing), "\n";
