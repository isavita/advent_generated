
#!/usr/bin/env raku
use v6;

my %cache;

sub count-arrangements-recursive(Str $springs, @group,
                                 Int $i-springs, Int $i-group,
                                 Int $contig-damaged) returns Int {
    my $len = $springs.chars;

    if $i-springs == $len {
        return 1
          if $i-group == @group.elems && $contig-damaged == 0;
        return 1
          if $i-group == @group.elems - 1
          && $contig-damaged == @group[$i-group];
        return 0;
    }

    my $key = "$i-springs|$i-group|$contig-damaged";
    return %cache{$key} if %cache{$key}:exists;

    my $c = $springs.substr($i-springs, 1);
    my $res = 0;

    # treat '.' or '?' as possible spring break
    if $c eq '.' || $c eq '?' {
        if $contig-damaged == 0 {
            $res += count-arrangements-recursive(
                $springs, @group,
                $i-springs + 1, $i-group, 0);
        }
        elsif $i-group < @group.elems
          && $contig-damaged == @group[$i-group] {
            $res += count-arrangements-recursive(
                $springs, @group,
                $i-springs + 1, $i-group + 1, 0);
        }
    }

    # treat '#' or '?' as possible damaged spring
    if $c eq '#' || $c eq '?' {
        if $i-group < @group.elems
          && $contig-damaged < @group[$i-group] {
            $res += count-arrangements-recursive(
                $springs, @group,
                $i-springs + 1, $i-group, $contig-damaged + 1);
        }
    }

    %cache{$key} = $res;
    $res;
}

sub count-arrangements(Str $springs, @group) returns Int {
    %cache = ();                     # clear memoization for each row
    count-arrangements-recursive($springs, @group, 0, 0, 0);
}

sub MAIN() {
    my $total = 0;
    for "input.txt".IO.lines -> $line {
        my ($springs, $group-str) = $line.split(' ', 2);
        my @group = $group-str.split(',').map(*.Int);
        $total += count-arrangements($springs, @group);
    }
    say $total;
}
