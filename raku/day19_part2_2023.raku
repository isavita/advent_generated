
class Rule {
    has Str $.category;
    has Str $.operator;
    has Int $.num;
    has Str $.workflow-name;
}

class Solver {
    has %!workflows;
    has Str $.start-workflow = 'in';
    has Int $.min-rating = 1;
    has Int $.max-rating = 4000;

    submethod BUILD(Str :$input-file) {
        my @input = $input-file.IO.lines;
        %!workflows = self!parse-workflows(@input);
    }

    method solve {
        my %initial = x => [1,4000], m => [1,4000], a => [1,4000], s => [1,4000];
        self!apply-workflow-interval(%initial, $!start-workflow);
    }

    method !parse-workflows(@input) {
        my %w;
        for @input -> $line {
            last if $line eq '';
            my ($name, $rest) = $line.split('{');
            $rest = $rest.substr(0, *-1);
            my @rules = $rest.split(',').map: -> $r {
                if $r.contains(':') {
                    my $cat = $r.substr(0,1);
                    my $op  = $r.substr(1,1);
                    my ($num, $target) = $r.substr(2).split(':');
                    Rule.new(category => $cat, operator => $op, num => +$num, workflow-name => $target);
                } else {
                    Rule.new(workflow-name => $r);
                }
            }
            %w{$name} = @rules;
        }
        %w
    }

    method !apply-workflow-interval(%part, Str $workflow) {
        return 0 if $workflow eq 'R';
        return self!calculate-combinations(%part) if $workflow eq 'A';
        my $total = 0;
        for %!workflows{$workflow}.flat -> $rule {
            unless $rule.operator {
                $total += self!apply-workflow-interval(%part, $rule.workflow-name);
                last;
            }
            my ($valid, $invalid) = self!split-interval(%part{$rule.category}, $rule);
            if $valid {
                my %new = %part;
                %new{$rule.category} = $valid;
                $total += self!apply-workflow-interval(%new, $rule.workflow-name);
            }
            last unless $invalid;
            %part{$rule.category} = $invalid;
        }
        $total
    }

    method !split-interval($interval, Rule $rule) {
        my ($s, $e) = $interval.list;
        given $rule.operator {
            when '>' {
                return (Nil, Nil) if $s > $rule.num;
                my $valid  = $rule.num + 1 <= $e ?? [$rule.num + 1, $e] !! Nil;
                my $invalid = $s <= $rule.num ?? [$s, $rule.num] !! Nil;
                return ($valid, $invalid);
            }
            when '<' {
                return (Nil, Nil) if $e < $rule.num;
                my $valid  = $s <= $rule.num - 1 ?? [$s, $rule.num - 1] !! Nil;
                my $invalid = $rule.num <= $e ?? [$rule.num, $e] !! Nil;
                return ($valid, $invalid);
            }
        }
    }

    method !calculate-combinations(%part) {
        [*] %part.values.map: -> ($s,$e) { $e - $s + 1 }
    }
}

sub MAIN {
    say Solver.new(input-file => 'input.txt').solve;
}
