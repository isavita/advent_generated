
sub main {
    my $stream = 'input.txt'.IO.slurp.chomp;

    my $total_score = 0;
    my $current_score = 0;
    my $in_garbage = False;
    my $ignore_next = False;

    for $stream.comb -> $char {
        if $ignore_next {
            $ignore_next = False;
        } elsif $char eq '!' {
            $ignore_next = True;
        } elsif $in_garbage {
            if $char eq '>' {
                $in_garbage = False;
            }
        } else { # not in_garbage
            if $char eq '<' {
                $in_garbage = True;
            } elsif $char eq '{' {
                $current_score++;
            } elsif $char eq '}' {
                $total_score += $current_score;
                $current_score--;
            }
        }
    }

    say $total_score;
}

main;
