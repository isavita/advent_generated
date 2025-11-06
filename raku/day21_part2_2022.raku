
class Monkey {
    has Str  $.name;
    has Int  $.val is rw;
    has Bool $.has-val is rw;
    has Monkey $.left is rw;
    has Monkey $.right is rw;
    has Str  $.op is rw;

    method new(Str $name) { self.bless(:$name, :val(0), :has-val(False)) }

    method solve() {
        return $!val, True if $!has-val;
        return 0, False without $!left || $!right;

        my ($l, $l-ok) = $!left.solve;
        my ($r, $r-ok) = $!right.solve;
        return 0, False unless $l-ok && $r-ok;

        given $!op {
            when '+'  { return $l + $r, True }
            when '-'  { return $l - $r, True }
            when '*'  { return $l * $r, True }
            when '/'  { return $l div $r, True }
            when '==' { return ($l == $r ?? 0 !! 1), True }
        }
        return 0, False;
    }

    method expect(Int $x) {
        return $x if $!name eq 'humn';

        my ($l, $l-ok) = $!left.solve;
        my ($r, $r-ok) = $!right.solve;

        unless $l-ok {
            given $!op {
                when '+'  { return $!left.expect($x - $r) }
                when '-'  { return $!left.expect($x + $r) }
                when '*'  { return $!left.expect($x div $r) }
                when '/'  { return $!left.expect($x * $r) }
                when '==' { return $!left.expect($r) }
            }
        }
        unless $r-ok {
            given $!op {
                when '+'  { return $!right.expect($x - $l) }
                when '-'  { return $!right.expect($l - $x) }
                when '*'  { return $!right.expect($x div $l) }
                when '/'  { return $!right.expect($l div $x) }
                when '==' { return $!right.expect($l) }
            }
        }
        die "impossible";
    }
}

sub parse() {
    my %index;
    for 'input.txt'.IO.lines {
        my ($goal, $rest) = .split(': ');
        %index{$goal} //= Monkey.new($goal);

        if $rest ~~ /^\d+$/ {
            %index{$goal}.val = +$rest;
            %index{$goal}.has-val = True;
            next;
        }

        my ($left, $op, $right) = $rest.split(' ');
        %index{$left}  //= Monkey.new($left);
        %index{$right} //= Monkey.new($right);

        %index{$goal}.left  = %index{$left};
        %index{$goal}.op    = $op;
        %index{$goal}.right = %index{$right};
    }
    %index;
}

sub MAIN() {
    my %m = parse;
    %m<humn>.has-val = False;
    %m<root>.op = '==';
    say %m<root>.expect(0);
}
