use strict;
use warnings;

# Open the input file
open my $fh, '<', 'input.txt' or die "Cannot open input.txt: $!";

my (@left_list, @right_list);

# Read the file line by line
while (my $line = <$fh>) {
    chomp $line;
    next if $line =~ /^\s*$/; # Skip empty lines
    my ($left, $right) = split /\s+/, $line;
    push @left_list, $left;
    push @right_list, $right;
}

close $fh;

# Count occurrences in the right list
my %right_count;
foreach my $num (@right_list) {
    $right_count{$num}++;
}

# Calculate the similarity score
my $similarity_score = 0;
foreach my $num (@left_list) {
    if (exists $right_count{$num}) {
        $similarity_score += $num * $right_count{$num};
    }
}

# Print the result
print "Similarity score: $similarity_score\n";
