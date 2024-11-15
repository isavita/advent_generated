
#!/usr/bin/perl
use strict;
use warnings;

# Read input from file
open(my $fh, '<', 'input.txt') or die "Cannot open file: $!";
my $input = do { local $/; <$fh> };
close $fh;

# Split input into patterns
my @patterns = split(/\n\n/, $input);

my $total_summary = 0;

foreach my $pattern (@patterns) {
    # Split pattern into rows
    my @rows = split(/\n/, $pattern);
    
    # Check vertical reflection
    my $vertical_reflection = find_vertical_reflection(\@rows);
    if (defined $vertical_reflection) {
        $total_summary += $vertical_reflection;
        next;
    }
    
    # Check horizontal reflection
    my $horizontal_reflection = find_horizontal_reflection(\@rows);
    if (defined $horizontal_reflection) {
        $total_summary += $horizontal_reflection * 100;
    }
}

print "$total_summary\n";

# Find vertical line of reflection
sub find_vertical_reflection {
    my ($rows) = @_;
    my $width = length($rows->[0]);
    
    for my $col (1..$width-1) {
        my $is_reflection = 1;
        
        for my $row (@$rows) {
            for my $offset (0..$width-1) {
                my $left = $col - $offset - 1;
                my $right = $col + $offset;
                
                last if $left < 0 || $right >= $width;
                
                if (substr($row, $left, 1) ne substr($row, $right, 1)) {
                    $is_reflection = 0;
                    last;
                }
            }
            
            last unless $is_reflection;
        }
        
        return $col if $is_reflection;
    }
    
    return undef;
}

# Find horizontal line of reflection
sub find_horizontal_reflection {
    my ($rows) = @_;
    my $height = scalar @$rows;
    
    for my $row (1..$height-1) {
        my $is_reflection = 1;
        
        for my $offset (0..$height-1) {
            my $top = $row - $offset - 1;
            my $bottom = $row + $offset;
            
            last if $top < 0 || $bottom >= $height;
            
            if ($rows->[$top] ne $rows->[$bottom]) {
                $is_reflection = 0;
                last;
            }
        }
        
        return $row if $is_reflection;
    }
    
    return undef;
}
