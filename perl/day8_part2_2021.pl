
#!/usr/bin/perl
use strict;
use warnings;
use List::Util qw(sum);

# Read input from file
open(my $fh, '<', 'input.txt') or die "Cannot open file: $!";

my $part1_count = 0;
my $part2_sum = 0;

while (my $line = <$fh>) {
    chomp $line;
    my ($patterns_str, $output_str) = split(' \| ', $line);
    my @patterns = split(' ', $patterns_str);
    my @output = split(' ', $output_str);

    # Part 1: Count unique segment digits
    $part1_count += scalar(grep { 
        length($_) == 2 || # 1
        length($_) == 4 || # 4
        length($_) == 3 || # 7
        length($_) == 7    # 8
    } @output);

    # Part 2: Decode digits
    my %digit_map = decode_digits(@patterns);
    my $output_value = decode_output(\%digit_map, @output);
    $part2_sum += $output_value;
}

close($fh);

print "Part 1: $part1_count\n";
print "Part 2: $part2_sum\n";

sub decode_digits {
    my @patterns = @_;
    my %digits;
    my %segments;

    # Identify known digits by length
    $digits{1} = (grep { length($_) == 2 } @patterns)[0];
    $digits{4} = (grep { length($_) == 4 } @patterns)[0];
    $digits{7} = (grep { length($_) == 3 } @patterns)[0];
    $digits{8} = (grep { length($_) == 7 } @patterns)[0];

    # Identify 3 (5-segment digit containing all of 1's segments)
    $digits{3} = (grep { 
        length($_) == 5 && 
        is_superset($_, $digits{1}) 
    } @patterns)[0];

    # Identify 6 (6-segment digit not containing all of 1's segments)
    $digits{6} = (grep { 
        length($_) == 6 && 
        !is_superset($_, $digits{1}) 
    } @patterns)[0];

    # Identify 9 (6-segment digit containing all of 4's segments)
    $digits{9} = (grep { 
        length($_) == 6 && 
        is_superset($_, $digits{4}) 
    } @patterns)[0];

    # Identify 0 (remaining 6-segment digit)
    $digits{0} = (grep { 
        length($_) == 6 && 
        $_ ne $digits{6} && 
        $_ ne $digits{9} 
    } @patterns)[0];

    # Identify 5 (5-segment digit fully contained in 6)
    $digits{5} = (grep { 
        length($_) == 5 && 
        is_subset($_, $digits{6}) 
    } @patterns)[0];

    # Identify 2 (remaining 5-segment digit)
    $digits{2} = (grep { 
        length($_) == 5 && 
        $_ ne $digits{3} && 
        $_ ne $digits{5} 
    } @patterns)[0];

    # Create reverse mapping for decoding
    my %reverse_map = map { 
        my $sorted = join('', sort(split(//, $digits{$_}))); 
        $sorted => $_ 
    } keys %digits;

    return %reverse_map;
}

sub decode_output {
    my ($digit_map, @output) = @_;
    my $value = join('', map { 
        my $sorted = join('', sort(split(//, $_))); 
        $digit_map->{$sorted} 
    } @output);
    return $value;
}

sub is_superset {
    my ($set, $subset) = @_;
    my %chars = map { $_ => 1 } split(//, $set);
    return !grep { !exists $chars{$_} } split(//, $subset);
}

sub is_subset {
    my ($subset, $set) = @_;
    return is_superset($set, $subset);
}
