
use strict;
use warnings;

my %index = ();

sub Monkey {
    my $self = {
        Name => $_[0],
        Val => 0,
        HasVal => 0,
        Left => undef,
        Right => undef,
        Op => ""
    };
    bless $self;
    return $self;
}

sub Solve {
    my ($m) = @_;
    if ($m->{HasVal}) {
        return ($m->{Val}, 1);
    }

    if ($m->{Left} && $m->{Right}) {
        my ($left, $lOk) = Solve($m->{Left});
        my ($right, $rOk) = Solve($m->{Right});

        if ($lOk && $rOk) {
            if ($m->{Op} eq "+") {
                return ($left + $right, 1);
            } elsif ($m->{Op} eq "-") {
                return ($left - $right, 1);
            } elsif ($m->{Op} eq "*") {
                return ($left * $right, 1);
            } elsif ($m->{Op} eq "/") {
                return ($left / $right, 1);
            } elsif ($m->{Op} eq "==") {
                if ($left == $right) {
                    return (0, 1);
                } else {
                    return (1, 1);
                }
            }
        }
    }
    return (0, 0);
}

sub Expect {
    my ($m, $x) = @_;
    if ($m->{Name} eq "humn") {
        return $x;
    }

    my ($left, $lOk) = Solve($m->{Left});
    my ($right, $rOk) = Solve($m->{Right});

    if (!$lOk) {
        if ($m->{Op} eq "+") {
            return Expect($m->{Left}, $x - $right);
        } elsif ($m->{Op} eq "-") {
            return Expect($m->{Left}, $x + $right);
        } elsif ($m->{Op} eq "*") {
            return Expect($m->{Left}, $x / $right);
        } elsif ($m->{Op} eq "/") {
            return Expect($m->{Left}, $x * $right);
        } elsif ($m->{Op} eq "==") {
            return Expect($m->{Left}, $right);
        }
    }

    if (!$rOk) {
        if ($m->{Op} eq "+") {
            return Expect($m->{Right}, $x - $left);
        } elsif ($m->{Op} eq "-") {
            return Expect($m->{Right}, $left - $x);
        } elsif ($m->{Op} eq "*") {
            return Expect($m->{Right}, $x / $left);
        } elsif ($m->{Op} eq "/") {
            return Expect($m->{Right}, $left / $x);
        } elsif ($m->{Op} eq "==") {
            return Expect($m->{Right}, $left);
        }
    }

    die "impossible";
}

sub parse {
    open(my $fh, '<', 'input.txt') or die $!;
    while (my $line = <$fh>) {
        chomp $line;
        my @ff = split(": ", $line);
        my $goal = $ff[0];

        if (!exists $index{$goal}) {
            $index{$goal} = Monkey($goal);
        }

        if ($ff[1] =~ /^\d+$/) {
            $index{$goal}->{Val} = $ff[1];
            $index{$goal}->{HasVal} = 1;
            next;
        }

        my @r = split(" ", $ff[1]);
        my ($left, $op, $right) = ($r[0], $r[1], $r[2]);

        if (!exists $index{$left}) {
            $index{$left} = Monkey($left);
        }

        if (!exists $index{$right}) {
            $index{$right} = Monkey($right);
        }

        $index{$goal}->{Left} = $index{$left};
        $index{$goal}->{Op} = $op;
        $index{$goal}->{Right} = $index{$right};
    }
}

parse();
$index{"humn"}->{HasVal} = 0;
$index{"root"}->{Op} = "==";

print Expect($index{"root"}, 0) . "\n";
