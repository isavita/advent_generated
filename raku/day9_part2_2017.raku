
use v6;

unit sub MAIN() {
    my $data = 'input.txt'.IO.slurp.trim;

    my $score = 0;
    my $garbage_count = 0;
    my Bool $in_garbage = False;
    my Bool $ignore_next = False;

    for $data.comb -> $char {
        if $ignore_next {
            $ignore_next = False;
        } elsif $char eq '!' {
            $ignore_next = True;
        } elsif $in_garbage {
            if $char eq '>' {
                $in_garbage = False;
            } else {
                $garbage_count++;
            }
        } else {
            if $char eq '<' {
                $in_garbage = True;
            } elsif $char eq '{' {
                $score++;
            }
        }
    }

    say $score;
    say $garbage_count;
}
