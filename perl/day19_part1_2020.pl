use strict;
use warnings;

sub read_rules {
    my %rules;
    while (<>) {
        chomp;
        last if $_ eq "";
        my ($key, $value) = split /:\s*/, $_, 2;
        $value =~ s/"//g;
        $rules{$key} = $value;
    }
    return \%rules;
}

sub construct_pattern {
    my ($rules, $index) = @_;
    if ($rules->{$index} =~ /\|/) {
        my @subrules = split /\s*\|\s*/, $rules->{$index};
        my @parts;
        foreach my $subrule (@subrules) {
            push @parts, construct_sub_pattern($rules, $subrule);
        }
        return "(" . join("|", @parts) . ")";
    }
    return construct_sub_pattern($rules, $rules->{$index});
}

sub construct_sub_pattern {
    my ($rules, $subrule) = @_;
    return $subrule if $subrule eq "a" || $subrule eq "b";
    my @sub_idxs = split /\s+/, $subrule;
    my $pattern = "";
    foreach my $idx (@sub_idxs) {
        $pattern .= construct_pattern($rules, $idx);
    }
    return $pattern;
}

sub count_matches {
    my ($pattern) = @_;
    my $count = 0;
    while (<>) {
        chomp;
        $count++ if /^$pattern$/;
    }
    return $count;
}

# Open the input file
open my $fh, "<", "input.txt" or die "Could not open file 'input.txt': $!";

# Set the default input filehandle to the opened file
local *ARGV = $fh;

# Read the rules from the input file
my $rules = read_rules();

# Construct the pattern for rule 0
my $pattern = construct_pattern($rules, 0);

# Reset the filehandle to the beginning of the file
seek $fh, 0, 0;

# Count the number of matches
my $count = count_matches($pattern);

# Print the result
print "The number of messages that completely match rule 0 is: $count\n";

# Close the file handle
close $fh;