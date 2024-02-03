
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die "Cannot open file: $!";
my $input = do { local $/; <$fh> };
close($fh);

my ($reverseGraph, $startingMols) = parseInput($input);

my %productToReactant;
for my $react (keys %$reverseGraph) {
    for my $p (@{$reverseGraph->{$react}}) {
        if (exists $productToReactant{$p}) {
            die "dup found";
        }
        $productToReactant{$p} = $react;
    }
}

my @allProducts = keys %productToReactant;
my $start = join('', @$startingMols);
my $mol = $start;
my $steps = 0;

while ($mol ne 'e') {
    my $changeMade = 0;
    for my $prod (@allProducts) {
        my $count = () = $mol =~ /$prod/g;
        if ($count <= 0) {
            next;
        }
        $changeMade = 1;
        $steps += $count;
        $mol =~ s/$prod/$productToReactant{$prod}/g;

        last;
    }

    if (!$changeMade) {
        @allProducts = shuffleSlice(\@allProducts);
        $mol = $start;
        $steps = 0;
    }
}

print "$steps\n";

sub parseInput {
    my ($input) = @_;
    my @blocks = split(/\n\n/, $input);
    my @startingMaterial = splitMolecules($blocks[1]);

    my %graph;

    for my $l (split(/\n/, $blocks[0])) {
        my @parts = split(' => ', $l);
        push @{$graph{$parts[0]}}, $parts[1];
    }

    return \%graph, \@startingMaterial;
}

sub splitMolecules {
    my ($input) = @_;
    my @molecules;
    my @chars = split('', $input);

    for my $char (@chars) {
        my $code = ord($char);
        if ($code >= 65 && $code <= 90) {
            push @molecules, $char;
        } else {
            $molecules[$#molecules] .= $char;
        }
    }

    return @molecules;
}

sub shuffleSlice {
    my ($in) = @_;
    @$in = sort { int(rand(2)) } @$in;
    return $in;
}
