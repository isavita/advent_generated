
my constant HIGH_CARD  = 1;
my constant ONE_PAIR   = 2;
my constant TWO_PAIR   = 3;
my constant THREE_KIND = 4;
my constant FULL_HOUSE = 5;
my constant FOUR_KIND  = 6;
my constant FIVE_KIND  = 7;

sub find-matches(@hands) {
    my @matches = [ [] xx 7 ];
    for @hands -> $hand {
        my %count;
        $hand<cards>.comb.map({ %count{$_}++ });
        my $prod = [*] %count.values;
        given $prod {
            when 1  { @matches[FIVE_KIND-1].push: $hand }
            when 2  { @matches[FOUR_KIND-1].push: $hand }
            when 3  { @matches[THREE_KIND-1].push: $hand }
            when 4  {
                %count == 2
                  ?? @matches[ONE_PAIR-1].push: $hand
                  !! @matches[FULL_HOUSE-1].push: $hand
            }
            when 5  { @matches[HIGH_CARD-1].push: $hand }
            when 6  { @matches[TWO_PAIR-1].push: $hand }
        }
    }
    @matches
}

sub convert-and-order-matches(@matches) {
    my @out;
    for @matches -> @cat {
        my @temp = @cat.map: -> $h {
            my $rank = $h<cards>.trans('TJQKA' => 'ABCDE').parse-base(16);
            %( hand => $h, rank => $rank )
        }
        @out.append: @temp.sort: -*<rank>;
    }
    @out
}

sub MAIN() {
    my @hands = 'input.txt'.IO.lines.map: -> $line {
        my ($c, $b) = $line.match(/^ (<[0..9A..Z]>+) \s+ (\d+) $/).list;
        %( cards => ~$c, bid => +$b )
    }

    my @ranked = convert-and-order-matches(find-matches(@hands));
    my $total = 0;
    for @ranked.kv -> $i, $r { $total += $r<hand><bid> * (@ranked - $i) }
    say $total
}
