use strict;
use warnings;

# Node represents an object in space.
package Node;
sub new {
    my ($class, $name) = @_;
    my $self = {
        name     => $name,
        children => [],
        parent   => undef,
    };
    bless $self, $class;
    return $self;
}

# findOrCreateNode finds or creates a node with the given name.
sub findOrCreateNode {
    my ($name, $nodes) = @_;
    return $nodes->{$name} if exists $nodes->{$name};
    my $node = Node->new($name);
    $nodes->{$name} = $node;
    return $node;
}

# buildOrbitMap builds the orbit map from the input data.
sub buildOrbitMap {
    my ($input) = @_;
    my %nodes;
    foreach my $line (@$input) {
        my ($center_name, $orbiter_name) = split /\)/, $line;
        my $center = findOrCreateNode($center_name, \%nodes);
        my $orbiter = findOrCreateNode($orbiter_name, \%nodes);
        push @{$center->{children}}, $orbiter;
        $orbiter->{parent} = $center;
    }
    return \%nodes;
}

# pathToRoot creates an array of nodes from a given node to the root.
sub pathToRoot {
    my ($node) = @_;
    my @path;
    while (defined $node) {
        push @path, $node;
        $node = $node->{parent};
    }
    return \@path;
}

# findCommonAncestor finds the common ancestor of two nodes and returns the paths to it.
sub findCommonAncestor {
    my ($node1, $node2) = @_;
    my $path1 = pathToRoot($node1);
    my $path2 = pathToRoot($node2);

    my $i = $#{$path1};
    my $j = $#{$path2};

    # Move upwards until the paths diverge.
    while ($i >= 0 && $j >= 0 && $path1->[$i] == $path2->[$j]) {
        $i--;
        $j--;
    }
    return ($i + 1, $j + 1);
}

# Read input from file
open my $fh, '<', 'input.txt' or die "Could not open file 'input.txt': $!";
my @input = <$fh>;
chomp @input;
close $fh;

my $orbitMap = buildOrbitMap(\@input);

# Calculate the number of orbital transfers.
my ($transfersYOU, $transfersSAN) = findCommonAncestor($orbitMap->{'YOU'}->{parent}, $orbitMap->{'SAN'}->{parent});
print $transfersYOU + $transfersSAN;