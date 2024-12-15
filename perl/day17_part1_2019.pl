
#!/usr/bin/perl
use strict;
use warnings;
use List::Util qw(sum);

sub decode {
    my ($n) = @_;
    my $op = $n % 100;
    $n = int($n / 100);
    my @modes = ();
    for (0..2) {
        push @modes, $n % 10;
        $n = int($n / 10);
    }
    return ($op, @modes);
}

sub get {
    my ($machine, $i, $mode) = @_;
    my $val;
    if ($mode == 1) {
        $val = $machine->{data}->[$i];
    } elsif ($mode == 0) {
        $val = $machine->{data}->[$machine->{data}->[$i]];
    } elsif ($mode == 2) {
        $val = $machine->{data}->[$machine->{relbase} + $machine->{data}->[$i]];
    } else {
        die "Unknown mode: $mode";
    }
    return $val;
}

sub set {
    my ($machine, $i, $mode, $val) = @_;
    if ($mode == 0) {
        $machine->{data}->[$machine->{data}->[$i]] = $val;
    } elsif ($mode == 2) {
        $machine->{data}->[$machine->{relbase} + $machine->{data}->[$i]] = $val;
    } else {
        die "Unknown mode: $mode";
    }
}

sub step {
    my ($machine) = @_;
    my ($op, @modes) = decode($machine->{data}->[$machine->{ip}]);
    if ($op == 1) {
        my $val = get($machine, $machine->{ip} + 1, $modes[0]) + get($machine, $machine->{ip} + 2, $modes[1]);
        set($machine, $machine->{ip} + 3, $modes[2], $val);
        $machine->{ip} += 4;
    } elsif ($op == 2) {
        my $val = get($machine, $machine->{ip} + 1, $modes[0]) * get($machine, $machine->{ip} + 2, $modes[1]);
        set($machine, $machine->{ip} + 3, $modes[2], $val);
        $machine->{ip} += 4;
    } elsif ($op == 3) {
        my $input = shift @{$machine->{input}};
        set($machine, $machine->{ip} + 1, $modes[0], $input);
        $machine->{ip} += 2;
    } elsif ($op == 4) {
        push @{$machine->{output}}, get($machine, $machine->{ip} + 1, $modes[0]);
        $machine->{ip} += 2;
    } elsif ($op == 5) {
        if (get($machine, $machine->{ip} + 1, $modes[0]) != 0) {
            $machine->{ip} = get($machine, $machine->{ip} + 2, $modes[1]);
        } else {
            $machine->{ip} += 3;
        }
    } elsif ($op == 6) {
        if (get($machine, $machine->{ip} + 1, $modes[0]) == 0) {
            $machine->{ip} = get($machine, $machine->{ip} + 2, $modes[1]);
        } else {
            $machine->{ip} += 3;
        }
    } elsif ($op == 7) {
        if (get($machine, $machine->{ip} + 1, $modes[0]) < get($machine, $machine->{ip} + 2, $modes[1])) {
            set($machine, $machine->{ip} + 3, $modes[2], 1);
        } else {
            set($machine, $machine->{ip} + 3, $modes[2], 0);
        }
        $machine->{ip} += 4;
    } elsif ($op == 8) {
        if (get($machine, $machine->{ip} + 1, $modes[0]) == get($machine, $machine->{ip} + 2, $modes[1])) {
            set($machine, $machine->{ip} + 3, $modes[2], 1);
        } else {
            set($machine, $machine->{ip} + 3, $modes[2], 0);
        }
        $machine->{ip} += 4;
    } elsif ($op == 9) {
        $machine->{relbase} += get($machine, $machine->{ip} + 1, $modes[0]);
        $machine->{ip} += 2;
    } elsif ($op == 99) {
        return 0;
    } else {
        die "Unknown opcode: $op";
    }
    return 1;
}

sub run {
    my ($machine) = @_;
    while (step($machine)) {}
    return $machine->{output};
}

sub new_machine {
    my ($program, $input) = @_;
    my %machine = (
        data => [@$program],
        ip => 0,
        input => $input,
        output => [],
        relbase => 0,
    );
    return \%machine;
}

sub parse {
    my ($program) = @_;
    my $machine = new_machine($program, []);
    my @output = @{run($machine)};
    my $sb = join "", map { chr } @output;
    my %scaffolding;
    my ($robot_x, $robot_y, $dir);
    my @lines = split /\n/, $sb;
    for my $y (0..$#lines) {
        for my $x (0..length($lines[$y])-1) {
            my $char = substr($lines[$y], $x, 1);
            if ($char =~ /[\^\>\<v]/) {
                $robot_x = $x;
                $robot_y = $y;
                $dir = {
                    '^' => 0,
                    '>' => 1,
                    'v' => 2,
                    '<' => 3,
                }->{$char};
                $scaffolding{"$x,$y"} = 1;
            } elsif ($char eq '#') {
                $scaffolding{"$x,$y"} = 1;
            }
        }
    }
    return (\%scaffolding, [$robot_x, $robot_y], $dir);
}

sub sum_align {
    my ($grid) = @_;
    my $sum = 0;
    for my $key (keys %$grid) {
        my ($x, $y) = split ',', $key;
        my $is_intersection = 1;
        for my $dxdy ([-1,0], [1,0], [0,-1], [0,1]) {
            my ($dx, $dy) = @$dxdy;
            if (!exists $grid->{"".($x+$dx).",".($y+$dy)}) {
                $is_intersection = 0;
                last;
            }
        }
        if ($is_intersection) {
            $sum += $x * $y;
        }
    }
    return $sum;
}

sub path {
    my ($scaffolding, $robot, $dir) = @_;
    my ($x, $y) = @$robot;
    my @dirs = ([0,1], [1,0], [0,-1], [-1,0]);
    my @sections;
    my $dist = 0;
    my $d;
    while (1) {
        my ($dx, $dy) = $dirs[$dir];
        if (exists $scaffolding->{"".($x-$dx).",".($y-$dy)}) {
            $x -= $dx;
            $y -= $dy;
            $dist++;
            next;
        }
        if ($dist > 0) {
            push @sections, "$d,$dist";
        }
        my $next_dir = ($dir + 1) % 4;
        my ($ndx, $ndy) = $dirs[$next_dir];
        if (exists $scaffolding->{"".($x-$ndx).",".($y-$ndy)}) {
            $x -= $ndx;
            $y -= $ndy;
            $dir = $next_dir;
            $dist = 1;
            $d = 'R';
        } else {
            my $prev_dir = ($dir + 3) % 4;
            my ($pdx, $pdy) = $dirs[$prev_dir];
            if (exists $scaffolding->{"".($x-$pdx).",".($y-$pdy)}) {
                $x -= $pdx;
                $y -= $pdy;
                $dir = $prev_dir;
                $dist = 1;
                $d = 'L';
            } else {
                last;
            }
        }
    }
    return join ",", @sections;
}

sub encode {
    my ($path) = @_;
    my ($seq, $a, $b, $c);
    my @path_parts = split ',', $path;
    for my $i (2..21) {
        for my $j (2..21) {
            for my $k (2..21) {
                my $next = $path . ",";
                $a = join ",", @path_parts[0..$i-1];
                $next =~ s/$a,?//g;
                my @next_parts = split ',', $next;
                $b = join ",", @next_parts[0..$j-1];
                $next =~ s/$b,?//g;
                @next_parts = split ',', $next;
                $c = join ",", @next_parts[0..$k-1];
                $next =~ s/$c,?//g;
                if ($next eq "") {
                    goto FOUND;
                }
            }
        }
    }
    FOUND:
    $a =~ s/,+$//;
    $b =~ s/,+$//;
    $c =~ s/,+$//;
    $path =~ s/$a/A/g;
    $path =~ s/$b/B/g;
    $path =~ s/$c/C/g;
    $path =~ s/,+$//;
    return ($path, $a, $b, $c);
}

sub read_all {
    my ($filepath) = @_;
    open my $fh, '<', $filepath or die "Could not open file '$filepath': $!";
    local $/ = undef;
    my $content = <$fh>;
    close $fh;
    chomp $content;
    return $content;
}

sub atoi {
    my ($s) = @_;
    return int($s);
}

my @program = map { atoi($_) } split ',', read_all("input.txt");
my ($scaffolding, $robot, $dir) = parse(\@program);
print sum_align($scaffolding), "\n";
