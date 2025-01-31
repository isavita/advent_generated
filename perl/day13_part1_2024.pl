
open my $f, '<', 'input.txt' or die $!;
my (@machines, @lines);
while (<$f>) {
    chomp;
    if ($_ eq '') {
        if (@lines) {
            push @machines, [parse_machine(\@lines)];
            @lines = ();
        }
    } else {
        push @lines, $_;
    }
}
if (@lines) {
    push @machines, [parse_machine(\@lines)];
}
close $f;

my (@results);
foreach my $m (@machines) {
    my $cost = solve_machine($m);
    if ($cost >= 0) {
        push @results, $cost;
    }
}

if (@results == 0) {
    print "0 0\n";
} else {
    my $count = @results;
    my $sum = 0;
    foreach my $c (@results) {
        $sum += $c;
    }
    print "$count $sum\n";
}

sub parse_machine {
    my $lines = shift;
    my ($ax, $ay, $bx, $by, $px, $py);
    foreach my $l (@$lines) {
        $l =~ s/Button A:/A:/g;
        $l =~ s/Button B:/B:/g;
        $l =~ s/Prize:/P:/g;
        if ($l =~ /^A:/) {
            ($ax, $ay) = parse_line(substr($l, 2));
        } elsif ($l =~ /^B:/) {
            ($bx, $by) = parse_line(substr($l, 2));
        } elsif ($l =~ /^P:/) {
            ($px, $py) = parse_prize(substr($l, 2));
        }
    }
    return ($ax, $ay, $bx, $by, $px, $py);
}

sub parse_line {
    my $s = shift;
    $s =~ s/^\s+|\s+$//g;
    my ($xp, $yp) = split /,/, $s;
    $xp =~ s/^\s+|\s+$//g;
    $yp =~ s/^\s+|\s+$//g;
    return (parse_val($xp), parse_val($yp));
}

sub parse_prize {
    my $s = shift;
    $s =~ s/^\s+|\s+$//g;
    my ($xp, $yp) = split /,/, $s;
    $xp =~ s/^\s+|\s+$//g;
    $yp =~ s/^\s+|\s+$//g;
    return (parse_val_prize($xp), parse_val_prize($yp));
}

sub parse_val {
    my $s = shift;
    $s =~ s/^\s+|\s+$//g;
    $s =~ s/^X\+//;
    $s =~ s/^Y\+//;
    $s =~ s/^X=//;
    $s =~ s/^Y=//;
    return $s;
}

sub parse_val_prize {
    my $s = shift;
    $s =~ s/^\s+|\s+$//g;
    $s =~ s/^X=//;
    $s =~ s/^Y=//;
    return $s;
}

sub solve_machine {
    my $m = shift;
    my ($ax, $ay, $bx, $by, $px, $py) = @$m;
    my $min_cost = -1;

    if ($ax == 0 && $bx == 0 && $px != 0) {
      return -1;
    }
    if ($ay == 0 && $by == 0 && $py != 0) {
      return -1;
    }
    
    if ($ax == 0 && $bx == 0 && $px == 0) {
       if ($ay == 0 && $by == 0 && $py == 0) {
           return 0;
       } elsif ($ay == 0) {
          if ($py % $by == 0) {
              my $b_count = $py / $by;
              if ($b_count <= 100) {
                 return $b_count
              } else {
                return -1;
              }
          } else {
            return -1
          }
       } elsif ($by == 0) {
          if ($py % $ay == 0) {
              my $a_count = $py / $ay;
              if ($a_count <= 100) {
                 return 3*$a_count;
              } else {
                return -1;
              }
          } else {
            return -1
          }
       }
    }
    
    if ($ax == 0 && $bx !=0 && $px % $bx == 0) {
        my $b_count_x = $px / $bx;
        if ($b_count_x <= 100) {
            if ($ay == 0) {
                if ($py % $by == 0) {
                    my $b_count_y = $py / $by;
                    if ($b_count_x == $b_count_y && $b_count_y <= 100) {
                        $min_cost = $b_count_y;
                    }
                }
            } else {
              for (my $a_count = 0; $a_count <= 100; $a_count++) {
                   my $y = $ay * $a_count + $by * $b_count_x;
                   if ($y == $py) {
                     my $cost = $a_count * 3 + $b_count_x;
                     if ($min_cost < 0 || $cost < $min_cost) {
                        $min_cost = $cost;
                     }
                   }
              }
            }
        }
    }

    if ($ay == 0 && $by !=0 && $py % $by == 0) {
        my $b_count_y = $py / $by;
        if ($b_count_y <= 100) {
           if ($ax == 0) {
              if ($px % $bx == 0) {
                my $b_count_x = $px / $bx;
                 if ($b_count_x == $b_count_y && $b_count_y <= 100) {
                       if ($min_cost < 0 || $b_count_y < $min_cost) {
                           $min_cost = $b_count_y;
                       }
                    }
              }
           } else {
                for (my $a_count = 0; $a_count <= 100; $a_count++) {
                    my $x = $ax * $a_count + $bx * $b_count_y;
                    if ($x == $px) {
                        my $cost = $a_count * 3 + $b_count_y;
                        if ($min_cost < 0 || $cost < $min_cost) {
                            $min_cost = $cost;
                        }
                    }
                }
            }
        }
    }

    for (my $a_count = 0; $a_count <= 100; $a_count++) {
        for (my $b_count = 0; $b_count <= 100; $b_count++) {
            my $x = $ax * $a_count + $bx * $b_count;
            my $y = $ay * $a_count + $by * $b_count;
            if ($x == $px && $y == $py) {
                my $cost = $a_count * 3 + $b_count;
                if ($min_cost < 0 || $cost < $min_cost) {
                    $min_cost = $cost;
                }
            }
        }
    }
    return $min_cost;
}
