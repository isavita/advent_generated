
open(my $fh, '<', 'input.txt') or die "Error opening file: $!";
my $moves = <$fh>;
close($fh);

my @moves = split(",", $moves);
my @programs = split("", "abcdefghijklmnop");
my $initial = join("", @programs);
my $cycleLen = 0;

for (my $i = 0; $i < 1000000000; $i++) {
    foreach my $move (@moves) {
        if ($move =~ /^s/) {
            my $x = substr($move, 1);
            spin(\@programs, $x);
        } elsif ($move =~ /^x/) {
            my @positions = split("/", substr($move, 1));
            my $A = $positions[0];
            my $B = $positions[1];
            exchange(\@programs, $A, $B);
        } elsif ($move =~ /^p/) {
            my @positions = split("/", substr($move, 1));
            my $A = $positions[0];
            my $B = $positions[1];
            partner(\@programs, $A, $B);
        }
    }

    if (join("", @programs) eq $initial) {
        $cycleLen = $i + 1;
        last;
    }
}

@programs = split("", $initial);

for (my $i = 0; $i < 1000000000 % $cycleLen; $i++) {
    foreach my $move (@moves) {
        if ($move =~ /^s/) {
            my $x = substr($move, 1);
            spin(\@programs, $x);
        } elsif ($move =~ /^x/) {
            my @positions = split("/", substr($move, 1));
            my $A = $positions[0];
            my $B = $positions[1];
            exchange(\@programs, $A, $B);
        } elsif ($move =~ /^p/) {
            my @positions = split("/", substr($move, 1));
            my $A = $positions[0];
            my $B = $positions[1];
            partner(\@programs, $A, $B);
        }
    }
}

print join("", @programs) . "\n";

sub spin {
    my ($programs, $x) = @_;
    my $n = scalar(@$programs);
    my @temp = @$programs;

    for (my $i = 0; $i < $n; $i++) {
        $programs->[($i + $x) % $n] = $temp[$i];
    }
}

sub exchange {
    my ($programs, $A, $B) = @_;
    ($programs->[$A], $programs->[$B]) = ($programs->[$B], $programs->[$A]);
}

sub partner {
    my ($programs, $A, $B) = @_;
    my ($indexA, $indexB);

    foreach my $i (0..$#{$programs}) {
        $indexA = $i if $programs->[$i] eq $A;
        $indexB = $i if $programs->[$i] eq $B;
    }

    exchange($programs, $indexA, $indexB);
}
