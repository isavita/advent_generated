
#!/usr/bin/perl
use strict;
use warnings;

my %map;
my $size;
my @movements;
my @dirs = ([-1, 0], [0, 1], [1, 0], [0, -1]);
my $facing = 1;
my $x = 0;
my $y;

open my $fh, '<', 'input.txt' or die "Could not open input.txt: $!";
my $r = 0;
while (my $line = <$fh>) {
    chomp $line;
    if ($line eq "") {
        last;
    }
    if ($r == 0) {
        $size = length($line) / 3;
        $y = $size;
    }
    for (my $c = 0; $c < length($line); $c++) {
        my $char = substr($line, $c, 1);
        next if $char eq ' ';
        $map{$r . "," . $c} = ($char eq '#') ? 1 : 0;
    }
    $r++;
}
my $path = <$fh>;
chomp $path;
close $fh;

my $acc = 0;
for my $char (split //, $path) {
    if ($char eq 'R' || $char eq 'L') {
        push @movements, { steps => $acc };
        $acc = 0;
        push @movements, { rotate => $char };
    } else {
        $acc = $acc * 10 + $char;
    }
}
push @movements, { steps => $acc };

for my $mov (@movements) {
    if (exists $mov->{rotate}) {
        $facing = ($facing + ($mov->{rotate} eq 'R' ? 1 : -1) + 4) % 4;
    } else {
        for (my $i = 0; $i < $mov->{steps}; $i++) {
            my $dir = $dirs[$facing];
            my $next_x = $x + $dir->[0];
            my $next_y = $y + $dir->[1];
            my $key = $next_x . "," . $next_y;
            if (exists $map{$key}) {
                if ($map{$key}) {
                    last;
                }
                $x = $next_x;
                $y = $next_y;
            } else {
                my ($opp_x, $opp_y) = (-$dir->[0], -$dir->[1]);
                while (1) {
                    $next_x += $opp_x;
                    $next_y += $opp_y;
                    my $look_ahead_key = $next_x . "," . $next_y;
                    if (!exists $map{$look_ahead_key}) {
                        my $curr_key = ($next_x - $opp_x) . "," . ($next_y - $opp_y);
                        if ($map{$curr_key}) {
                            last;
                        }
                        $x = $next_x - $opp_x;
                        $y = $next_y - $opp_y;
                        last;
                    }
                }
            }
        }
    }
}

my $points = ($facing + 3) % 4;
print 1000 * ($x + 1) + 4 * ($y + 1) + $points, "\n";
