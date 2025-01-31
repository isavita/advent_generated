
use strict;
use warnings;
use Storable qw(dclone);

my %positionCache;
my %okCache;
my %moveCache;
my %solveCache;

sub find_position {
    my ($mat, $ch) = @_;
    my $key = $ch . join("", @$mat);
    return $positionCache{$key} if exists $positionCache{$key};

    for my $i (0 .. @$mat - 1) {
        for my $j (0 .. length($mat->[$i]) - 1) {
            if (substr($mat->[$i], $j, 1) eq $ch) {
                my $pos = [$i, $j];
                $positionCache{$key} = $pos;
                return $pos;
            }
        }
    }
    return [-1, -1];
}

sub ok {
    my ($mat, $st, $seq) = @_;
    my $key = join(",", @$st, $seq) . join("", @$mat);
    return $okCache{$key} if exists $okCache{$key};

    my ($i, $j) = @$st;
    for my $ch (split //, $seq) {
        if (substr($mat->[$i], $j, 1) eq ' ') {
            $okCache{$key} = 0;
            return 0;
        }
        $i += $ch eq 'v' ? 1 : ($ch eq '^' ? -1 : 0);
        $j += $ch eq '>' ? 1 : ($ch eq '<' ? -1 : 0);

        if ($i < 0 || $i >= @$mat || $j < 0 || $j >= length($mat->[0])) {
            $okCache{$key} = 0;
            return 0;
        }
    }
    $okCache{$key} = 1;
    return 1;
}

sub generate_moves {
    my ($position, $objective, $pad) = @_;
    my $key = join(",", @$position, $objective) . join("", @$pad);
    return $moveCache{$key} if exists $moveCache{$key};

    my $objPos = find_position($pad, $objective);
    my $ret = "";
    $ret .= "<" x ($position->[1] - $objPos->[1]) if $position->[1] > $objPos->[1];
    $ret .= "^" x ($position->[0] - $objPos->[0]) if $position->[0] > $objPos->[0];
    $ret .= "v" x ($objPos->[0] - $position->[0]) if $position->[0] < $objPos->[0];
    $ret .= ">" x ($objPos->[1] - $position->[1]) if $position->[1] < $objPos->[1];

    my $result = $ret;
    unless (ok($pad, $position, $result)) {
        $ret = "";
        $ret .= ">" x ($objPos->[1] - $position->[1]) if $position->[1] < $objPos->[1];
        $ret .= "^" x ($position->[0] - $objPos->[0]) if $position->[0] > $objPos->[0];
        $ret .= "v" x ($objPos->[0] - $position->[0]) if $position->[0] < $objPos->[0];
        $ret .= "<" x ($position->[1] - $objPos->[1]) if $position->[1] > $objPos->[1];
        $result = $ret;
    }
    $moveCache{$key} = $result;
    return $result;
}

sub solve {
    my ($code, $robots, $keyPad, $robotPad, $maxRobots) = @_;
    my $key = join(",", $code, $robots, $maxRobots);
    return $solveCache{$key} if exists $solveCache{$key};

    return length($code) if $robots <= 0;

    my $ret = 0;
    my ($posi, $posj) = (3, 2);
    $posi = 0 if $robots != $maxRobots;

    my $moves;
    for my $i (0 .. length($code) - 1) {
        my $ch = substr($code, $i, 1);
        if ($robots == $maxRobots) {
            $moves = generate_moves([$posi, $posj], $ch, $keyPad);
            my $pos = find_position($keyPad, $ch);
            ($posi, $posj) = @$pos;
        } else {
            $moves = generate_moves([$posi, $posj], $ch, $robotPad);
            my $pos = find_position($robotPad, $ch);
            ($posi, $posj) = @$pos;
        }
        $ret += solve($moves . "A", $robots - 1, $keyPad, $robotPad, $maxRobots);
    }
    $solveCache{$key} = $ret;
    return $ret;
}

sub main {
    open(my $fh, '<', 'input.txt') or die "Could not open file 'input.txt' $!";
    my $content = do { local $/; <$fh> };
    close($fh);

    my $maxRobots = 26;
    my @keyPad = ("789", "456", "123", " 0A");
    my @robotPad = (" ^A", "<v>");

    my $ret = 0;
    for my $code (split /\s+/, $content) {
        next if $code eq "";

        my $numericPart = 0;
        $numericPart = $numericPart * 10 + $1 while $code =~ /([0-9])/g;

        my $sv = solve($code, $maxRobots, \@keyPad, \@robotPad, $maxRobots);
        $ret += $sv * $numericPart;
    }
    print "$ret\n";
}

main();
