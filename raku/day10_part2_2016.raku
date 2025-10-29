
use v6;

sub MAIN() {
    my @lines = "input.txt".IO.lines;
    my @bots;
    my %outputs;
    for @lines -> $line {
        given $line {
            when / ^ 'value' \s+ (\d+) \s+ 'goes to bot' \s+ (\d+) $/ {
                my $v = +$0; my $b = +$1;
                @bots[$b] //= { lowTo => [], highTo => [], chips => [] };
                @bots[$b]<chips>.push($v);
            }
            when / ^ 'bot' \s+ (\d+) \s+ 'gives low to' \s+ (bot|output) \s+ (\d+) \s+ 'and high to' \s+ (bot|output) \s+ (\d+) $/ {
                my $b = +$0;
                my $lt = ~$1; my $li = +$2;
                my $ht = ~$3; my $hi = +$4;
                @bots[$b] //= { lowTo => [], highTo => [], chips => [] };
                @bots[$b]<lowTo>  = [$lt, $li];
                @bots[$b]<highTo> = [$ht, $hi];
            }
        }
    }
    my $again = True;
    while $again {
        $again = False;
        for @bots.kv -> $i, $bot {
            next unless $bot && $bot<chips>.elems == 2;
            $again = True;
            my @s = $bot<chips>.sort;
            my ($lt, $li) = $bot<lowTo>.list;
            my ($ht, $hi) = $bot<highTo>.list;
            if $lt eq 'bot' {
                @bots[$li] //= { lowTo => [], highTo => [], chips => [] };
                @bots[$li]<chips>.push(@s[0]);
            } else {
                %outputs{$li} = @s[0];
            }
            if $ht eq 'bot' {
                @bots[$hi] //= { lowTo => [], highTo => [], chips => [] };
                @bots[$hi]<chips>.push(@s[1]);
            } else {
                %outputs{$hi} = @s[1];
            }
            $bot<chips> = [];
        }
    }
    say %outputs{0} * %outputs{1} * %outputs{2};
}
