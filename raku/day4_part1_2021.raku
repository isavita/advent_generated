
#!/usr/bin/env raku

class BingoBoard {
    has @.numbers;
    has @!marked;

    submethod BUILD(:@!numbers) {
        @!marked = [ [ False xx 5 ] for ^5 ];
    }

    method mark(Int $number) {
        for 0..4 -> $r {
            for 0..4 -> $c {
                if @!numbers[$r][$c] == $number {
                    @!marked[$r][$c] = True;
                    return;
                }
            }
        }
    }

    method has-won() returns Bool {
        for @!marked -> @row {
            return True if @row.all;
        }

        for 0..4 -> $c {
            return True if (@!marked.map(*[$c])).all;
        }

        False
    }

    method unmarked-sum() returns Int {
        my $sum = 0;
        for 0..4 -> $r {
            for 0..4 -> $c {
                $sum += @!numbers[$r][$c] unless @!marked[$r][$c];
            }
        }
        $sum
    }
}

sub MAIN() {
    my @lines = "input.txt".IO.lines;

    my @calls = @lines.shift.split(',').map(*.Int);
    @lines.shift while @lines.elems && @lines[0].trim eq '';

    my @boards;
    while @lines.elems >= 5 {
        my @board-data;
        for ^5 {
            @board-data.push: @lines.shift.split(' ').grep(*.chars).map(*.Int);
        }
        @boards.push: BingoBoard.new(:numbers(@board-data));
        @lines.shift while @lines.elems && @lines[0].trim eq '';
    }

    my ($winning-board, $winning-number);

    for @calls -> $call {
        for @boards -> $board {
            $board.mark($call);
            if $board.has-won {
                $winning-board = $board;
                $winning-number = $call;
                last;
            }
        }
        if $winning-board {
            last;
        }
    }

    say $winning-board.unmarked-sum * $winning-number;
}
