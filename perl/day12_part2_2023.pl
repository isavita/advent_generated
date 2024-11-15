
use strict;
use warnings;
use Memoize;

memoize('count_arrangements_recursive');

sub parse_input {
    my @input = @_;
    my @rows;
    for my $line (@input) {
        my ($springs, $numbers) = split ' ', $line;
        my @group = split ',', $numbers;
        push @rows, { springs => $springs, group => \@group };
    }
    return @rows;
}

sub count_arrangements_recursive {
    my ($row, $i_springs, $i_group, $i_contiguous_damaged, $cache) = @_;
    my $key = "$i_springs,$i_group,$i_contiguous_damaged";
    return $cache->{$key} if exists $cache->{$key};

    my $res = 0;
    if ($i_springs == length($row->{springs})) {
        $res = 1 if $i_group == @{$row->{group}} && $i_contiguous_damaged == 0;
        $res = 1 if $i_group == @{$row->{group}} - 1 && $i_contiguous_damaged == $row->{group}[$i_group];
    } else {
        my $char = substr($row->{springs}, $i_springs, 1);
        if ($char eq '.' || $char eq '?') {
            $res += count_arrangements_recursive($row, $i_springs + 1, $i_group, 0, $cache) if $i_contiguous_damaged == 0;
            $res += count_arrangements_recursive($row, $i_springs + 1, $i_group + 1, 0, $cache) if $i_contiguous_damaged == $row->{group}[$i_group];
        }
        if ($char eq '#' || $char eq '?') {
            $res += count_arrangements_recursive($row, $i_springs + 1, $i_group, $i_contiguous_damaged + 1, $cache) if $i_group < @{$row->{group}} && $i_contiguous_damaged < $row->{group}[$i_group];
        }
    }
    $cache->{$key} = $res;
    return $res;
}

sub count_arrangements {
    my ($row) = @_;
    return count_arrangements_recursive($row, 0, 0, 0, {});
}

sub unfold_row {
    my ($row, $unfolding_factor) = @_;
    my $new_springs = $row->{springs};
    my @new_group = @{$row->{group}};
    for (1 .. $unfolding_factor - 1) {
        $new_springs .= '?' . $row->{springs};
        push @new_group, @{$row->{group}};
    }
    return { springs => $new_springs, group => \@new_group };
}

sub solve {
    my @input = @_;
    my @rows = parse_input(@input);
    my @unfolded_rows = map { unfold_row($_, 5) } @rows;
    my $res = 0;
    $res += count_arrangements($_) for @unfolded_rows;
    return $res;
}

open my $fh, '<', 'input.txt' or die "Cannot open file: $!";
my @input = <$fh>;
close $fh;
chomp @input;

print solve(@input), "\n";
