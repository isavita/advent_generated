
use v6.d;

my %value = J => 1, 2 => 2, 3 => 3, 4 => 4, 5 => 5, 6 => 6, 7 => 7, 8 => 8, 9 => 9, T => 10, Q => 11, K => 12, A => 13;

my @matches = [] xx 7;

for 'input.txt'.IO.lines {
    next unless .chars;
    my ($cards, $bid) = .comb(/ <[A..Z0..9]>+ /);
    my %count;
    %count{$_}++ for $cards.comb;
    if %count<J> {
        my ($high, $key) = 0, 'J';
        for %count.kv -> $k,$v {
            next if $k eq 'J';
            if $v > $high || ($v == $high && %value{$k} > %value{$key}) {
                ($high, $key) = $v, $k;
            }
        }
        if $key ne 'J' {
            %count{$key} += %count<J>;
            %count<J>:delete;
        }
    }
    my $val = [*] %count.values;
    my $idx = do given $val {
        when 1  { 6 }
        when 2  { 5 }
        when 3  { 3 }
        when 4  { %count == 2 ?? 1 !! 4 }
        when 5  { 0 }
        when 6  { 2 }
        default { die "oops" }
    }
    @matches[$idx].push: [$cards, $bid];
}

my @flat;
for @matches -> @group {
    my @temp;
    for @group -> ($c,$b) {
        my $s = $c.trans(['A','T','J','Q','K'] => ['E','A','1','C','D']);
        @temp.push: [:16($s), $b];
    }
    @flat.append: @temp.sort({ -$_[0] });
}

my $total = 0;
for @flat.kv -> $i, ($_,$b) { $total += $b * (@flat - $i) }

put $total;
