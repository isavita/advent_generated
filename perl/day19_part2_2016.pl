use strict;
use warnings;

sub readFile {
    my ($filename) = @_;
    open my $fh, '<', $filename or die "Cannot open file: $!";
    my $content = do { local $/; <$fh> };
    close $fh;
    chomp $content;
    return $content;
}

my $input = readFile("input.txt");
my $ans = elephant($input);
print "$ans\n";

sub elephant {
    my ($input) = @_;
    my $startingElves = int($input);
    my $root = { elfNum => 1, presents => 1 };
    my $iter = $root;

    for my $i (2 .. $startingElves) {
        $iter->{next} = { elfNum => $i, presents => 1 };
        $iter = $iter->{next};
    }
    $iter->{next} = $root;

    my $isOddLength = $startingElves % 2 == 1;
    my $beforeAcross = $root;

    for (0 .. int($startingElves/2) - 2) {
        $beforeAcross = $beforeAcross->{next};
    }

    while ($root->{next} != $root) {
        $root->{presents} += $beforeAcross->{next}{presents};
        $beforeAcross->{next} = $beforeAcross->{next}{next};

        if ($isOddLength) {
            $beforeAcross = $beforeAcross->{next};
        }
        $isOddLength = !$isOddLength;
        $root = $root->{next};
    }

    return $root->{elfNum};
}