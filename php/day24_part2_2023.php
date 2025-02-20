<?php
class Fraction {
  public $num;
  public $denom;
  function __construct($n, $d = 1) {
    $d = gmp_init($d);
    if(gmp_cmp($d, 0) == 0) throw new Exception("Division by zero");
    $n = gmp_init($n);
    $g = gmp_gcd($n, $d);
    $this->num = gmp_div_q($n, $g);
    $this->denom = gmp_div_q($d, $g);
    if(gmp_cmp($this->denom, 0) < 0) {
      $this->num = gmp_neg($this->num);
      $this->denom = gmp_neg($this->denom);
    }
  }
  function add($other) {
    $num = gmp_add(gmp_mul($this->num, $other->denom), gmp_mul($other->num, $this->denom));
    $denom = gmp_mul($this->denom, $other->denom);
    return new Fraction(gmp_strval($num), gmp_strval($denom));
  }
  function sub($other) {
    $num = gmp_sub(gmp_mul($this->num, $other->denom), gmp_mul($other->num, $this->denom));
    $denom = gmp_mul($this->denom, $other->denom);
    return new Fraction(gmp_strval($num), gmp_strval($denom));
  }
  function mul($other) {
    $num = gmp_mul($this->num, $other->num);
    $denom = gmp_mul($this->denom, $other->denom);
    return new Fraction(gmp_strval($num), gmp_strval($denom));
  }
  function div($other) {
    if(gmp_cmp($other->num, 0) == 0) throw new Exception("Division by zero");
    $num = gmp_mul($this->num, $other->denom);
    $denom = gmp_mul($this->denom, $other->num);
    return new Fraction(gmp_strval($num), gmp_strval($denom));
  }
  function negate() {
    return new Fraction(gmp_strval(gmp_neg($this->num)), gmp_strval($this->denom));
  }
  function toFloat() {
    return floatval(gmp_strval($this->num)) / floatval(gmp_strval($this->denom));
  }
  function toInt() {
    return intval(gmp_div_q($this->num, $this->denom));
  }
  function __toString() {
    if(gmp_cmp($this->denom, 1) == 0) return gmp_strval($this->num);
    return gmp_strval($this->num) . '/' . gmp_strval($this->denom);
  }
}

class RatVec3 {
  public $X, $Y, $Z;
  function __construct($x, $y, $z) { $this->X = $x; $this->Y = $y; $this->Z = $z; }
  function Add($other) { return new RatVec3($this->X->add($other->X), $this->Y->add($other->Y), $this->Z->add($other->Z)); }
  function Subtract($other) { return new RatVec3($this->X->sub($other->X), $this->Y->sub($other->Y), $this->Z->sub($other->Z)); }
  function Multiply($s) { return new RatVec3($this->X->mul($s), $this->Y->mul($s), $this->Z->mul($s)); }
  function Divide($s) { return new RatVec3($this->X->div($s), $this->Y->div($s), $this->Z->div($s)); }
  function Cross($other) {
    return new RatVec3(
      $this->Y->mul($other->Z)->sub($this->Z->mul($other->Y)),
      $this->Z->mul($other->X)->sub($this->X->mul($other->Z)),
      $this->X->mul($other->Y)->sub($this->Y->mul($other->X))
    );
  }
  function Dot($other) {
    return $this->X->mul($other->X)->add($this->Y->mul($other->Y))->add($this->Z->mul($other->Z));
  }
}

class HailStone {
  public $p, $v;
  function __construct($p, $v) { $this->p = $p; $this->v = $v; }
  function Subtract($other) { return new HailStone($this->p->Subtract($other->p), $this->v->Subtract($other->v)); }
}

function readFileLines($fileName) { return file($fileName, FILE_IGNORE_NEW_LINES); }
function solve($input) {
  $hailStones = readInput(array_slice($input, 0, 3));
  $s1 = $hailStones[1]; $s2 = $hailStones[2];
  $ref1 = $s1->Subtract($hailStones[0]); $ref2 = $s2->Subtract($hailStones[0]);
  $t1 = intersectionTime($ref2, $ref1); $t2 = intersectionTime($ref1, $ref2);
  $rock1 = $s1->p->Add($s1->v->Multiply($t1));
  $rock2 = $s2->p->Add($s2->v->Multiply($t2));
  $rp = $rock1->Subtract($rock2->Subtract($rock1)->Divide($t2->sub($t1))->Multiply($t1));
  $sum = $rp->X->add($rp->Y)->add($rp->Z);
  return (string)$sum->toInt();
}
function readInput($input) { $res = []; foreach ($input as $line) $res[] = parseLine($line); return $res; }
function parseLine($line) {
  preg_match_all('/-?\d+/', $line, $matches);
  $m = array_map('intval', $matches[0]);
  return new HailStone(
    new RatVec3(new Fraction($m[0]), new Fraction($m[1]), new Fraction($m[2])),
    new RatVec3(new Fraction($m[3]), new Fraction($m[4]), new Fraction($m[5]))
  );
}
function countIntersections($stones, $start, $end) {
  $count = 0; $zero = new Fraction(0,1);
  for ($i = 0; $i < count($stones); $i++)
    for ($j = $i+1; $j < count($stones); $j++) {
      list($x, $y, $t1, $t2, $ok) = doMatch2D($stones[$i], $stones[$j]);
      if ($ok && $t1->toFloat() > 0 && $t2->toFloat() >= 0 &&
          $x->toFloat() >= $start && $y->toFloat() >= $start &&
          $x->toFloat() <= $end && $y->toFloat() <= $end) $count++;
    }
  return $count;
}
function doMatch2D($a, $b) {
  $m1 = $a->v->Y->div($a->v->X);
  $m2 = $b->v->Y->div($b->v->X);
  if(gmp_cmp(gmp_mul($m1->num, $m2->denom), gmp_mul($m2->num, $m1->denom)) == 0)
    return [null,null,null,null,false];
  $numerator = $m1->mul($a->p->X)->sub($a->p->Y)->sub($m2->mul($b->p->X))->add($b->p->Y);
  $denom = $m1->sub($m2);
  $x = $numerator->div($denom);
  $y = $m1->mul($x->sub($a->p->X))->add($a->p->Y);
  $t1 = $x->sub($a->p->X)->div($a->v->X);
  $t2 = $x->sub($b->p->X)->div($b->v->X);
  return [$x, $y, $t1, $t2, true];
}
function intersectionTime($r, $s) {
  $plane = $r->p->Cross($r->p->Add($r->v));
  return $s->p->Dot($plane)->negate()->div($s->v->Dot($plane));
}
function addFrac($a, $b) { return $a->add($b); }
function subFrac($a, $b) { return $a->sub($b); }
function mulFrac($a, $b) { return $a->mul($b); }
function quoFrac($a, $b) { return $a->div($b); }
function toIntFrac($s) { return $s->toInt(); }
function r($rat) { return new Fraction($rat); }
function main() { $input = readFileLines("input.txt"); echo solve($input); }
main();
?>

