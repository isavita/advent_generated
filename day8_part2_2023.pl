
use strict;
use warnings;

sub parseInput {
    my @input = @_;
    my $instructions = $input[0];

    my %nodes = ();
    for my $line (@input[2..$#input]) {
        my ($head, @children) = parseLine($line);
        $nodes{$head} = \@children;
    }

    my %network = (
        Instructions => $instructions,
        Nodes => \%nodes,
    );
    return \%network;
}

sub parseLine {
    my $line = shift;
    my @parts = split(" = ", $line);

    my $head = $parts[0];
    my $childrenTrim = substr($parts[1], 1, -1);
    my @childrenParts = split(", ", $childrenTrim);
    my @children = ($childrenParts[0], $childrenParts[1]);

    return ($head, @children);
}

sub gcd {
    my ($a, $b) = @_;
    while ($b != 0) {
        ($a, $b) = ($b, $a % $b);
    }
    return $a;
}

sub lcm {
    my ($a, $b) = @_;
    return ($a * $b) / gcd($a, $b);
}

sub lcmSlice {
    my @nums = @_;
    if (@nums == 0) {
        return 0;
    }

    my $res = $nums[0];
    for my $i (1..$#nums) {
        $res = lcm($res, $nums[$i]);
    }

    return $res;
}

sub solve {
    my @input = @_;
    my $network = parseInput(@input);

    my @starts = ();
    for my $node (keys %{$network->{Nodes}}) {
        my $lastIndex = length($node) - 1;
        if (substr($node, $lastIndex, 1) eq 'A') {
            push @starts, $node;
        }
    }

    my @steps = ();
    my $instructionsLength = length($network->{Instructions});
    for my $i (0..$#starts) {
        my $element = $starts[$i];
        my $lastIndex = length($element) - 1;
        while (substr($element, $lastIndex, 1) ne 'Z') {
            my $instruction = substr($network->{Instructions}, $steps[$i] % $instructionsLength, 1);
            if ($instruction eq 'L') {
                $element = $network->{Nodes}->{$element}[0];
            } else {
                $element = $network->{Nodes}->{$element}[1];
            }
            $steps[$i]++;
        }
    }

    my $res = lcmSlice(@steps);
    return $res;
}

sub readFile {
    my $fileName = shift;
    open(my $fh, '<', $fileName) or die "Cannot open file: $!";
    my @lines = <$fh>;
    close($fh);
    chomp @lines;
    return @lines;
}

my @input = readFile("input.txt");
print solve(@input) . "\n";
