
use strict;
use warnings;

my @track;
my @carts;

open(my $fh, '<', 'input.txt') or die $!;
while (my $line = <$fh>) {
    chomp $line;
    my @chars = split(//, $line);
    push @track, [];
    foreach my $j (0..$#chars) {
        if ($chars[$j] eq '>') {
            push @{$track[-1]}, '-';
            my %cart = (x => $j, y => $#track, dir => '>', turn => 0);
            push @carts, \%cart;
        } elsif ($chars[$j] eq '<') {
            push @{$track[-1]}, '-';
            my %cart = (x => $j, y => $#track, dir => '<', turn => 0);
            push @carts, \%cart;
        } elsif ($chars[$j] eq '^') {
            push @{$track[-1]}, '|';
            my %cart = (x => $j, y => $#track, dir => '^', turn => 0);
            push @carts, \%cart;
        } elsif ($chars[$j] eq 'v') {
            push @{$track[-1]}, '|';
            my %cart = (x => $j, y => $#track, dir => 'v', turn => 0);
            push @carts, \%cart;
        } else {
            push @{$track[-1]}, $chars[$j];
        }
    }
}
close($fh);

my $collision = 0;
while (!$collision) {
    foreach my $i (0..$#carts) {
        if ($carts[$i]->{dir} eq '>') {
            $carts[$i] = moving_right(\@track, $carts[$i]);
        } elsif ($carts[$i]->{dir} eq '<') {
            $carts[$i] = moving_left(\@track, $carts[$i]);
        } elsif ($carts[$i]->{dir} eq '^') {
            $carts[$i] = moving_up(\@track, $carts[$i]);
        } elsif ($carts[$i]->{dir} eq 'v') {
            $carts[$i] = moving_down(\@track, $carts[$i]);
        } else {
            print "error not valid cart\n";
        }
    }

    foreach my $i (0..$#carts) {
        foreach my $j ($i+1..$#carts) {
            if ($carts[$i]->{x} == $carts[$j]->{x} && $carts[$i]->{y} == $carts[$j]->{y}) {
                $collision = 1;
                print "$carts[$i]->{x},$carts[$i]->{y}\n";
            }
        }
    }
}

sub print_track {
    my ($track, $carts) = @_;
    my @h;

    foreach my $i (0..$#{$track}) {
        push @h, [@{$track->[$i]}];
    }

    foreach my $cart (@{$carts}) {
        $h[$cart->{y}][$cart->{x}] = $cart->{dir};
    }

    foreach my $row (@h) {
        foreach my $s (@{$row}) {
            print "$s";
        }
        print "\n";
    }
}

sub moving_down {
    my ($track, $cart) = @_;
    if ($track->[$cart->{y}+1][$cart->{x}] eq '/') {
        $cart->{dir} = '<';
    } elsif ($track->[$cart->{y}+1][$cart->{x}] eq '\\') {
        $cart->{dir} = '>';
    } elsif ($track->[$cart->{y}+1][$cart->{x}] eq '+') {
        if ($cart->{turn} == 0) {
            $cart->{dir} = '>';
            $cart->{turn} = 1;
        } elsif ($cart->{turn} == 1) {
            $cart->{turn} = 2;
        } elsif ($cart->{turn} == 2) {
            $cart->{dir} = '<';
            $cart->{turn} = 0;
        }
    }
    $cart->{y} += 1;
    return $cart;
}

sub moving_up {
    my ($track, $cart) = @_;
    if ($track->[$cart->{y}-1][$cart->{x}] eq '/') {
        $cart->{dir} = '>';
    } elsif ($track->[$cart->{y}-1][$cart->{x}] eq '\\') {
        $cart->{dir} = '<';
    } elsif ($track->[$cart->{y}-1][$cart->{x}] eq '+') {
        if ($cart->{turn} == 0) {
            $cart->{dir} = '<';
            $cart->{turn} = 1;
        } elsif ($cart->{turn} == 1) {
            $cart->{turn} = 2;
        } elsif ($cart->{turn} == 2) {
            $cart->{dir} = '>';
            $cart->{turn} = 0;
        }
    }
    $cart->{y} -= 1;
    return $cart;
}

sub moving_left {
    my ($track, $cart) = @_;
    if ($track->[$cart->{y}][$cart->{x}-1] eq '/') {
        $cart->{dir} = 'v';
    } elsif ($track->[$cart->{y}][$cart->{x}-1] eq '\\') {
        $cart->{dir} = '^';
    } elsif ($track->[$cart->{y}][$cart->{x}-1] eq '+') {
        if ($cart->{turn} == 0) {
            $cart->{dir} = 'v';
            $cart->{turn} = 1;
        } elsif ($cart->{turn} == 1) {
            $cart->{turn} = 2;
        } elsif ($cart->{turn} == 2) {
            $cart->{dir} = '^';
            $cart->{turn} = 0;
        }
    }
    $cart->{x} -= 1;
    return $cart;
}

sub moving_right {
    my ($track, $cart) = @_;
    if ($track->[$cart->{y}][$cart->{x}+1] eq '\\') {
        $cart->{dir} = 'v';
    } elsif ($track->[$cart->{y}][$cart->{x}+1] eq '/') {
        $cart->{dir} = '^';
    } elsif ($track->[$cart->{y}][$cart->{x}+1] eq '+') {
        if ($cart->{turn} == 0) {
            $cart->{dir} = '^';
            $cart->{turn} = 1;
        } elsif ($cart->{turn} == 1) {
            $cart->{turn} = 2;
        } elsif ($cart->{turn} == 2) {
            $cart->{dir} = 'v';
            $cart->{turn} = 0;
        }
    }
    $cart->{x} += 1;
    return $cart;
}
