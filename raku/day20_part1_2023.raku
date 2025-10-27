
sub MAIN() {
    my %module;
    my @queue;
    my $low = 0;
    my $high = 0;

    for 'input.txt'.IO.lines {
        my ($left, $right) = .split(' -> ');
        my @dest = $right.split(', ');
        my ($type, $name) = $left.substr(0,1) eq 'b'
            ?? ('b', 'broadcaster')
            !! ($left.substr(0,1), $left.substr(1));
        %module{$name} = {
            type  => $type,
            dest  => @dest,
            state => False,
            mem   => {}
        };
    }

    for %module.kv -> $name, $m {
        for $m<dest>.flat -> $d {
            %module{$d} //= { type => 'o', dest => [], mem => {} };
            %module{$d}<type> eq '&' and
                %module{$d}<mem>{$name} = 0;
        }
    }

    for ^1000 {
        @queue.push( ['broadcaster', 0, 'button'] );
        while @queue {
            my ($curr, $pulse, $from) = @queue.shift;
            $pulse ?? ++$high !! ++$low;

            my $m := %module{$curr} // next;
            my $send = $pulse;

            given $m<type> {
                when '%' {
                    next if $pulse;
                    $m<state> = !$m<state>;
                    $send = $m<state> ?? 1 !! 0;
                }
                when '&' {
                    $m<mem>{$from} = $pulse;
                    $send = (all $m<mem>.values) ?? 0 !! 1;
                }
                when 'b'   { }
                default    { next }
            }

            for $m<dest>.flat -> $d {
                @queue.push( [$d, $send, $curr] );
            }
        }
    }

    say $low * $high;
}
