#!/usr/bin/env raku

class Bot {
    has Int @.chips;
    has Int $.low-type is rw = 0;
    has Int $.low-id   is rw = 0;
    has Int $.high-type is rw = 0;
    has Int $.high-id  is rw = 0;
    method add-chip(Int $c) { @.chips.push($c) }
    method clear-chips { @.chips = () }
}

sub MAIN {
    my $fh = open "input.txt", :r or die "Cannot open input.txt: $!";
    my %bots;
    my %outputs;
    my @queue;
    my $head = 0;
    for $fh.lines -> $line {
        my $l = $line.trim;
        next unless $l;
        if $l.starts-with("value") {
            if $l ~~ / ^ "value" \s+ (\d+) \s+ "goes to" \s+ "bot" \s+ (\d+) / {
                my $val = +$0;
                my $bid = +$1;
                my $b = %bots{$bid} //= Bot.new;
                $b.add-chip($val);
                @queue.push($bid) if $b.chips.elems == 2;
            } else { die "bad value line: $l" }
        } elsif $l.starts-with("bot") {
            if $l ~~ / ^ "bot" \s+ (\d+) \s+ "gives" \s+ "low" \s+ "to" \s+ (\w+) \s+ (\d+) \s+ "and" \s+ "high" \s+ "to" \s+ (\w+) \s+ (\d+) / {
                my $bid = +$0;
                my $lt = $1 eq "output" ?? 1 !! 0;
                my $lid = +$2;
                my $ht = $3 eq "output" ?? 1 !! 0;
                my $hid = +$4;
                my $b = %bots{$bid} //= Bot.new;
                $b.low-type = $lt;
                $b.low-id   = $lid;
                $b.high-type = $ht;
                $b.high-id  = $hid;
            } else { die "bad bot line: $l" }
        }
    }
    $fh.close;

    my $target = -1;
    while $head < @queue.elems {
        my $id = @queue[$head++];
        my $bot = %bots{$id} // next;
        next unless $bot.chips.elems == 2;
        my ($low, $high) = $bot.chips.sort;
        $target = $id if $low == 17 && $high == 61;
        if $bot.low-type == 0 {
            my $r = %bots{$bot.low-id} //= Bot.new;
            $r.add-chip($low);
            @queue.push($bot.low-id) if $r.chips.elems == 2;
        } else {
            %outputs{$bot.low-id} = $low;
        }
        if $bot.high-type == 0 {
            my $r = %bots{$bot.high-id} //= Bot.new;
            $r.add-chip($high);
            @queue.push($bot.high-id) if $r.chips.elems == 2;
        } else {
            %outputs{$bot.high-id} = $high;
        }
        $bot.clear-chips;
    }
    say $target;
}