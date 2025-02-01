
#!/usr/bin/perl
use strict;
use warnings;
use List::Util qw(sum max);

sub abs {
    my $a = shift;
    return $a < 0 ? -$a : $a;
}

sub distance {
    my ($c1, $c2) = @_;
    return abs($c1->[0] - $c2->[0]) + abs($c1->[1] - $c2->[1]) + abs($c1->[2] - $c2->[2]);
}

sub new_bots {
    my @input = @_;
    my %bots;
    for my $data (@input) {
        my ($x, $y, $z, $r) = $data =~ /pos=<\s*(-?\d+)\s*,\s*(-?\d+)\s*,\s*(-?\d+)\s*>, r=(-?\d+)/;
        push @{$bots{"$x,$y,$z"}}, $r;
    }
    return \%bots;
}

sub have_in_range {
    my ($bots, $pos) = @_;
    my $count = 0;
    for my $c (keys %$bots) {
        my ($x,$y,$z) = split ',', $c;
        my @rs = @{$bots->{$c}};
        for my $r (@rs) {
           if (distance($pos, [$x, $y, $z]) <= $r) {
                $count++;
            }
        }
    }
    return $count;
}

sub closest_success {
    my $bots = shift;
    my $zoom = 1 << 30; # Use 30 bits for zoom initially
    my @cur = (0, 0, 0);
    my @topLeft = (0,0,0);
    my @bottomRight = (0,0,0);

    while ($zoom > 0) {
        my %zoomed_bots;
         for my $c (keys %$bots) {
            my ($x,$y,$z) = split ',', $c;
            my @rs = @{$bots->{$c}};
            for my $r (@rs) {
               my $zc = join ',', int($x/$zoom), int($y/$zoom), int($z/$zoom);
               push @{$zoomed_bots{$zc}}, int($r/$zoom);
            }
        }


        my ($best_x,$best_y,$best_z) = (0,0,0);
        my $best_count = -1;

        for (my $x = $topLeft[0]; $x <= $bottomRight[0]; $x++) {
            for (my $y = $topLeft[1]; $y <= $bottomRight[1]; $y++) {
                for (my $z = $topLeft[2]; $z <= $bottomRight[2]; $z++) {
                     my $c = have_in_range(\%zoomed_bots, [$x,$y,$z]);

                     if ($c < $best_count){
                         next;
                     }
                     my $zero_distance = distance([0,0,0],[$x,$y,$z]);
                     my $best_zero_distance = distance([0,0,0],[$best_x,$best_y,$best_z]);

                     if ($c == $best_count && $zero_distance >= $best_zero_distance){
                          next;
                     }
                    ($best_x,$best_y,$best_z) = ($x,$y,$z);
                    $best_count = $c;
                }
            }
        }

        $topLeft[0] = ($best_x-1)*2;
        $topLeft[1] = ($best_y-1)*2;
        $topLeft[2] = ($best_z-1)*2;

        $bottomRight[0] = ($best_x+1)*2;
        $bottomRight[1] = ($best_y+1)*2;
        $bottomRight[2] = ($best_z+1)*2;


        $zoom >>= 1;

        if ($zoom == 0) {
            return distance([0, 0, 0], [$best_x,$best_y,$best_z]);
        }
    }
}

sub main {
    open my $fh, '<', "input.txt" or die "Could not open input.txt: $!";
    my @lines;
    while (my $line = <$fh>) {
        chomp $line;
        push @lines, $line;
    }
    close $fh;
    my $bots = new_bots(@lines);
    print closest_success($bots), "\n";
}

main();
