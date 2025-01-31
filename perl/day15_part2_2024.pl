
use strict;
use warnings;

my $Up    = [0, -1];
my $Down  = [0, 1];
my $Left  = [-1, 0];
my $Right = [1, 0];

sub parse {
    my ($input) = @_;
    my @blocks = split /\n\n/, $input;
    my @lines  = split /\n/, $blocks[0];
    my %m;
    for my $y ( 0 .. $#lines ) {
        for my $x ( 0 .. length( $lines[$y] ) - 1 ) {
            $m{ $x . "," . $y } = substr( $lines[$y], $x, 1 );
        }
    }
    my @steps;
    my $moves = $blocks[1];
    $moves =~ s/\n//g;

    for my $ch ( split //, $moves ) {
        if ( $ch eq '^' ) {
            push @steps, $Up;
        }
        elsif ( $ch eq '<' ) {
            push @steps, $Left;
        }
        elsif ( $ch eq '>' ) {
            push @steps, $Right;
        }
        elsif ( $ch eq 'v' ) {
            push @steps, $Down;
        }
    }
    return \%m, \@steps;
}

sub scaleUp {
    my ($input) = @_;
    $input =~ s/#/##/g;
    $input =~ s/\./../g;
    $input =~ s/O/\[\]/g;
    $input =~ s/\@/\@./g;
    return $input;
}

sub tryToStep {
    my ( $m, $pos, $dir ) = @_;
    my @pos = split /,/, $pos;
    my %orig = %$m;
    if ( $m->{$pos} eq '.' ) {
        return 1;
    }
    elsif ( $m->{$pos} eq 'O' || $m->{$pos} eq '@' ) {
        if ( tryToStep( $m, ( $pos[0] + $dir->[0] ) . "," . ( $pos[1] + $dir->[1] ), $dir ) ) {
            $m->{ ( $pos[0] + $dir->[0] ) . "," . ( $pos[1] + $dir->[1] ) } = $m->{$pos};
            $m->{$pos} = '.';
            return 1;
        }
    }
    elsif ( $m->{$pos} eq ']' ) {
        if ( tryToStep( $m, ( $pos[0] + $Left->[0] ) . "," . ( $pos[1] + $Left->[1] ), $dir ) ) {
            return 1;
        }
    }
    elsif ( $m->{$pos} eq '[' ) {
        if ( $dir eq $Left ) {
            if (
                tryToStep(
                    $m,
                    ( $pos[0] + $Left->[0] ) . "," . ( $pos[1] + $Left->[1] ),
                    $dir
                )
              )
            {
                $m->{ ( $pos[0] + $Left->[0] ) . "," . ( $pos[1] + $Left->[1] ) } = '[';
                $m->{$pos} = ']';
                $m->{ ( $pos[0] + $Right->[0] ) . "," . ( $pos[1] + $Right->[1] ) } = '.';
                return 1;
            }
        }
        elsif ( $dir eq $Right ) {
            if (
                tryToStep(
                    $m,
                    ( $pos[0] + 2 * $Right->[0] ) . "," . ( $pos[1] + 2 * $Right->[1] ),
                    $dir
                )
              )
            {
                $m->{$pos} = '.';
                $m->{ ( $pos[0] + $Right->[0] ) . "," . ( $pos[1] + $Right->[1] ) } = '[';
                $m->{ ( $pos[0] + 2 * $Right->[0] ) . "," . ( $pos[1] + 2 * $Right->[1] ) } =
                  ']';
                return 1;
            }
        }
        else {
            if (
                tryToStep( $m, ( $pos[0] + $dir->[0] ) . "," . ( $pos[1] + $dir->[1] ), $dir )
                && tryToStep(
                    $m,
                    ( $pos[0] + $Right->[0] + $dir->[0] ) . "," . ( $pos[1] + $Right->[1] + $dir->[1] ),
                    $dir
                )
              )
            {
                $m->{$pos} = '.';
                $m->{ ( $pos[0] + $Right->[0] ) . "," . ( $pos[1] + $Right->[1] ) } = '.';
                $m->{ ( $pos[0] + $dir->[0] ) . "," . ( $pos[1] + $dir->[1] ) } = '[';
                $m->{ ( $pos[0] + $dir->[0] + $Right->[0] ) . "," . ( $pos[1] + $dir->[1] + $Right->[1] ) }
                  = ']';
                return 1;
            }
        }
    }
    %$m = %orig;
    return 0;
}

sub solve {
    my ($input) = @_;
    my ( $m, $steps ) = parse($input);
    my $robot;
    for my $k ( keys %$m ) {
        if ( $m->{$k} eq '@' ) {
            $robot = $k;
            last;
        }
    }
    for my $dir (@$steps) {
      my @pos = split /,/, $robot;
        if ( tryToStep( $m, $robot, $dir ) ) {
          $robot = ( $pos[0] + $dir->[0] ) . "," . ( $pos[1] + $dir->[1] ) ;
        }
    }
    my $sum = 0;
    for my $k ( keys %$m ) {
        if ( $m->{$k} eq '[' || $m->{$k} eq 'O' ) {
          my @pos = split /,/, $k;
            $sum += $pos[0] + 100 * $pos[1];
        }
    }
    return $sum;
}

open my $fh, '<', 'input.txt' or die "Error reading input: $!";
my $input = do { local $/; <$fh> };
close $fh;

print solve($input) . "\n";
print solve( scaleUp($input) ) . "\n";
