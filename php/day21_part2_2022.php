
<?php

class Monkey {
    public $name;
    public $val;
    public $hasVal;
    public $left;
    public $right;
    public $op;

    public function solve() {
        if ($this->hasVal) {
            return [$this->val, true];
        }

        if ($this->left != null && $this->right != null) {
            $left = $this->left->solve();
            $right = $this->right->solve();

            if ($left[1] && $right[1]) {
                switch ($this->op) {
                    case "+":
                        return [$left[0] + $right[0], true];
                    case "-":
                        return [$left[0] - $right[0], true];
                    case "*":
                        return [$left[0] * $right[0], true];
                    case "/":
                        return [$left[0] / $right[0], true];
                    case "==":
                        if ($left[0] == $right[0]) {
                            return [0, true];
                        } else {
                            return [1, true];
                        }
                }
            }
        }
        return [0, false];
    }

    public function expect($x) {
        if ($this->name == "humn") {
            return $x;
        }

        $left = $this->left->solve();
        $right = $this->right->solve();

        if (!$left[1]) {
            switch ($this->op) {
                case "+":
                    return $this->left->expect($x - $right[0]);
                case "-":
                    return $this->left->expect($x + $right[0]);
                case "*":
                    return $this->left->expect($x / $right[0]);
                case "/":
                    return $this->left->expect($x * $right[0]);
                case "==":
                    return $this->left->expect($right[0]);
            }
        }

        if (!$right[1]) {
            switch ($this->op) {
                case "+":
                    return $this->right->expect($x - $left[0]);
                case "-":
                    return $this->right->expect($left[0] - $x);
                case "*":
                    return $this->right->expect($x / $left[0]);
                case "/":
                    return $this->right->expect($left[0] / $x);
                case "==":
                    return $this->right->expect($left[0]);
            }
        }
        throw new Exception("Impossible");
    }
}

function parse() {
    $index = [];

    $input = file_get_contents("input.txt");
    $lines = explode("\n", $input);

    foreach ($lines as $line) {
        $ff = explode(": ", $line);
        $goal = $ff[0];
        if (!array_key_exists($goal, $index)) {
            $index[$goal] = new Monkey();
            $index[$goal]->name = $goal;
        }

        if (is_numeric($ff[1])) {
            $index[$goal]->val = intval($ff[1]);
            $index[$goal]->hasVal = true;
            continue;
        }

        $r = explode(" ", $ff[1]);
        $left = $r[0];
        $op = $r[1];
        $right = $r[2];

        if (!array_key_exists($left, $index)) {
            $index[$left] = new Monkey();
            $index[$left]->name = $left;
        }

        if (!array_key_exists($right, $index)) {
            $index[$right] = new Monkey();
            $index[$right]->name = $right;
        }

        $index[$goal]->left = $index[$left];
        $index[$goal]->op = $op;
        $index[$goal]->right = $index[$right];
    }

    return $index;
}

$index = parse();

$index["humn"]->hasVal = false;
$index["root"]->op = "==";

echo $index["root"]->expect(0) . "\n";
?>
