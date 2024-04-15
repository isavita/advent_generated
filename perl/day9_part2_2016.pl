use strict;
use warnings;

open(my $file, '<', 'input.txt') or die "Could not open file 'input.txt': $!";
my $input = <$file>;
chomp $input;
close($file);

my $decompressed_length = get_decompressed_length_v2($input);
print "$decompressed_length\n";

sub get_decompressed_length_v2 {
    my ($input) = @_;
    return decompress($input, 0, length($input));
}

sub decompress {
    my ($input, $start, $end) = @_;
    my $length = 0;
    my $i = $start;
    while ($i < $end) {
        if (substr($input, $i, 1) eq '(') {
            my $marker_end = index($input, ')', $i);
            my ($char_count, $repeat_count) = split('x', substr($input, $i + 1, $marker_end - $i - 1));
            my $next_index = $marker_end + 1;
            $length += $repeat_count * decompress($input, $next_index, $next_index + $char_count);
            $i = $next_index + $char_count;
        } else {
            $length++;
            $i++;
        }
    }
    return $length;
}