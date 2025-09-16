#!/usr/bin/perl
use strict;
use warnings;

my $MIN_COORD = 200000000000000;
my $MAX_COORD = 400000000000000;

sub parse_line {
  my ($line) = @_;
  if ($line =~ /(-?\d+),\s*(-?\d+),\s*(-?\d+)\s*@\s*(-?\d+),\s*(-?\d+),\s*(-?\d+)/) {
    return { px => 0+$1, py => 0+$2, pz => 0+$3, vx => 0+$4, vy => 0+$5, vz => 0+$6 };
  }
  return undef;
}

sub read_hailstones {
  my ($filename) = @_;
  open my $fh, '<', $filename or die "Error: Could not open file '$filename'\n";
  my @hail;
  while (my $line = <$fh>) {
    chomp $line;
    my $h = parse_line($line);
    push @hail, $h if defined $h;
  }
  close $fh;
  return \@hail;
}

sub calculate_xy_intersection {
  my ($h1, $h2) = @_;
  my $denom = $h2->{vx} * $h1->{vy} - $h1->{vx} * $h2->{vy};
  return () if abs($denom) < 1e-12;
  my $t1 = ( ($h2->{px} - $h1->{px}) * (-$h2->{vy}) - (-$h2->{vx}) * ($h2->{py} - $h1->{py}) ) / $denom;
  my $t2 = ( $h1->{vx} * ($h2->{py} - $h1->{py}) - ($h2->{px} - $h1->{px}) * $h1->{vy} ) / $denom;
  my $ix = $h1->{px} + $h1->{vx} * $t1;
  my $iy = $h1->{py} + $h1->{vy} * $t1;
  return ($ix, $iy, $t1, $t2);
}

sub solve_part1 {
  my ($hailstones) = @_;
  my $count = 0;
  my $n = scalar(@$hailstones);
  for my $i (0 .. $n-2) {
    for my $j ($i+1 .. $n-1) {
      my ($ix, $iy, $t1, $t2) = calculate_xy_intersection($hailstones->[$i], $hailstones->[$j]);
      if (defined $ix && defined $t1 && defined $t2 && $t1 > 0 && $t2 > 0) {
        if ($ix >= $MIN_COORD && $ix <= $MAX_COORD && $iy >= $MIN_COORD && $iy <= $MAX_COORD) {
          $count++;
        }
      }
    }
  }
  return $count;
}

sub gaussian_elimination {
  my ($A, $b) = @_;
  my $n = scalar(@$A);
  for (my $j = 0; $j < $n; $j++) {
    my $pivot_row = $j;
    my $maxval = abs($A->[$j][$j]);
    for (my $i = $j+1; $i < $n; $i++) {
      if (abs($A->[$i][$j]) > $maxval) { $maxval = abs($A->[$i][$j]); $pivot_row = $i; }
    }
    if ($pivot_row != $j) {
      my $tmp = $A->[$j];
      $A->[$j] = $A->[$pivot_row];
      $A->[$pivot_row] = $tmp;
      my $tb = $b->[$j];
      $b->[$j] = $b->[$pivot_row];
      $b->[$pivot_row] = $tb;
    }
    my $pivot_val = $A->[$j][$j];
    return undef if abs($pivot_val) < 1e-12;
    for (my $k = $j; $k < $n; $k++) { $A->[$j][$k] /= $pivot_val; }
    $b->[$j] /= $pivot_val;
    $A->[$j][$j] = 1.0;
    for (my $i = 0; $i < $n; $i++) {
      next if $i == $j;
      my $factor = $A->[$i][$j];
      for (my $k = $j; $k < $n; $k++) {
        $A->[$i][$k] -= $factor * $A->[$j][$k];
      }
      $b->[$i] -= $factor * $b->[$j];
      $A->[$i][$j] = 0.0;
    }
  }
  return $b;
}

sub fill_row {
  my ($row, $hi, $hj, $plane, $A, $b) = @_;
  my $dvx = $hi->{vx} - $hj->{vx};
  my $dvy = $hi->{vy} - $hj->{vy};
  my $dvz = $hi->{vz} - $hj->{vz};
  my $dpx = $hi->{px} - $hj->{px};
  my $dpy = $hi->{py} - $hj->{py};
  my $dpz = $hi->{pz} - $hj->{pz};
  my $const;
  if ($plane eq 'xy') {
    $A->[$row][0] = -$dvy;
    $A->[$row][1] = $dvx;
    $A->[$row][2] = 0;
    $A->[$row][3] = -$dpy;
    $A->[$row][4] = $dpx;
    $A->[$row][5] = 0;
    $const = ($hj->{px} * $hj->{vy} - $hj->{py} * $hj->{vx}) - ($hi->{px} * $hi->{vy} - $hi->{py} * $hi->{vx});
  } elsif ($plane eq 'xz') {
    $A->[$row][0] = -$dvz;
    $A->[$row][1] = 0;
    $A->[$row][2] = $dvx;
    $A->[$row][3] = -$dpz;
    $A->[$row][4] = 0;
    $A->[$row][5] = $dpx;
    $const = ($hj->{px} * $hj->{vz} - $hj->{pz} * $hj->{vx}) - ($hi->{px} * $hi->{vz} - $hi->{pz} * $hi->{vx});
  } else {
    $A->[$row][0] = 0;
    $A->[$row][1] = -$dvz;
    $A->[$row][2] = $dvy;
    $A->[$row][3] = 0;
    $A->[$row][4] = -$dpz;
    $A->[$row][5] = $dpy;
    $const = ($hj->{py} * $hj->{vz} - $hj->{pz} * $hj->{vy}) - ($hi->{py} * $hi->{vz} - $hi->{pz} * $hi->{vy});
  }
  $b->[$row] = $const;
}

sub solve_part2 {
  my ($hailstones) = @_;
  my $n = scalar(@$hailstones);
  die "Need at least 4 hailstones\n" if $n < 4;
  my $h0 = $hailstones->[0];
  my $h1 = $hailstones->[1];
  my $h2 = $hailstones->[2];
  my $h3 = $hailstones->[3];
  my @A;
  for my $i (0..5) { $A[$i] = [0,0,0,0,0,0]; }
  my @b = (0) x 6;
  fill_row(0, $h0, $h1, 'xy', \@A, \@b);
  fill_row(1, $h0, $h2, 'xy', \@A, \@b);
  fill_row(2, $h0, $h3, 'xy', \@A, \@b);
  fill_row(3, $h0, $h1, 'xz', \@A, \@b);
  fill_row(4, $h0, $h2, 'xz', \@A, \@b);
  fill_row(5, $h0, $h3, 'xz', \@A, \@b);
  my $sol = gaussian_elimination(\@A, \@b);
  die "Failed to solve linear system\n" unless defined $sol;
  my $rx = int($sol->[0] + 0.5);
  my $ry = int($sol->[1] + 0.5);
  my $rz = int($sol->[2] + 0.5);
  return $rx + $ry + $rz;
}

my $hailstones = read_hailstones("input.txt");
my $part1 = solve_part1($hailstones);
my $part2 = solve_part2($hailstones);
print "$part1\n";
print "$part2\n";