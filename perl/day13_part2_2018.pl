
#!/usr/bin/perl
use strict;
use warnings;

my @tracks;
my @carts;

open my $fh, "<", "input.txt" or die "Could not open input.txt: $!";
my $y = 0;
while (my $line = <$fh>) {
    chomp $line;
    my @track_line = split //, $line;
    for my $x (0..$#track_line) {
        my $char = $track_line[$x];
        if ($char =~ /[><^v]/) {
            push @carts, { x => $x, y => $y, direction => $char, turns => 0 };
            $track_line[$x] = ($char =~ /[><]/) ? '-' : '|';
        }
    }
    push @tracks, \@track_line;
    $y++;
}
close $fh;

while (@carts > 1) {
    @carts = sort {
        $a->{y} <=> $b->{y} || $a->{x} <=> $b->{x}
    } @carts;

    my %to_remove;
    for my $i (0..$#carts) {
        next if exists $to_remove{$i};
        move_cart($carts[$i], \@tracks);
        my $crash_index = check_crash($carts[$i], \@carts);
        if ($crash_index != -1) {
            $to_remove{$i} = 1;
            $to_remove{$crash_index} = 1;
        }
    }

    my @new_carts;
    for my $i (0..$#carts) {
        push @new_carts, $carts[$i] unless exists $to_remove{$i};
    }
    @carts = @new_carts;
}

print $carts[0]->{x} . "," . $carts[0]->{y} . "\n";

sub move_cart {
    my ($cart, $tracks) = @_;
    my $dir = $cart->{direction};
    if ($dir eq '>') {
        $cart->{x}++;
    } elsif ($dir eq '<') {
        $cart->{x}--;
    } elsif ($dir eq '^') {
        $cart->{y}--;
    } else {
        $cart->{y}++;
    }

    my $track = $tracks->[$cart->{y}][$cart->{x}];
    if ($track eq '+') {
        turn_cart($cart);
    } elsif ($track eq '/' || $track eq '\\') {
        change_direction($cart, $track);
    }
}

sub turn_cart {
    my $cart = shift;
    my $turns = $cart->{turns};
    my $dir = $cart->{direction};
    if ($turns % 3 == 0) {
        if ($dir eq '>') {
            $cart->{direction} = '^';
        } elsif ($dir eq '<') {
            $cart->{direction} = 'v';
        } elsif ($dir eq '^') {
            $cart->{direction} = '<';
        } else {
            $cart->{direction} = '>';
        }
    } elsif ($turns % 3 == 2) {
        if ($dir eq '>') {
            $cart->{direction} = 'v';
        } elsif ($dir eq '<') {
            $cart->{direction} = '^';
        } elsif ($dir eq '^') {
            $cart->{direction} = '>';
        } else {
            $cart->{direction} = '<';
        }
    }
    $cart->{turns}++;
}

sub change_direction {
    my ($cart, $track) = @_;
    my $dir = $cart->{direction};
    if ($track eq '/') {
        if ($dir eq '>') {
            $cart->{direction} = '^';
        } elsif ($dir eq '<') {
            $cart->{direction} = 'v';
        } elsif ($dir eq '^') {
            $cart->{direction} = '>';
        } else {
            $cart->{direction} = '<';
        }
    } elsif ($track eq '\\') {
        if ($dir eq '>') {
            $cart->{direction} = 'v';
        } elsif ($dir eq '<') {
            $cart->{direction} = '^';
        } elsif ($dir eq '^') {
            $cart->{direction} = '<';
        } else {
            $cart->{direction} = '>';
        }
    }
}

sub check_crash {
    my ($cart, $carts) = @_;
    for my $i (0..$#{$carts}) {
        if ($carts->[$i] != $cart && $carts->[$i]->{x} == $cart->{x} && $carts->[$i]->{y} == $cart->{y}) {
            return $i;
        }
    }
    return -1;
}
