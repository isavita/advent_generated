<?php
function solve($buttons, $targets){
    $n = count($targets);
    $numButtons = count($buttons);
    $matrix = array_fill(0,$n,array_fill(0,$numButtons+1,0.0));
    for($j=0;$j<$n;$j++) $matrix[$j][$numButtons] = $targets[$j];
    foreach($buttons as $i=>$b){
        foreach($b as $j){
            if($j<$n) $matrix[$j][$i] = 1.0;
        }
    }
    $pivotCol = array_fill(0,$n,-1);
    $r = 0;
    for($c=0;$c<$numButtons && $r<$n;$c++){
        $mr = $r;
        for($i=$r+1;$i<$n;$i++) if(abs($matrix[$i][$c])>abs($matrix[$mr][$c])) $mr=$i;
        if(abs($matrix[$mr][$c])<1e-9) continue;
        $tmp = $matrix[$r]; $matrix[$r] = $matrix[$mr]; $matrix[$mr] = $tmp;
        $s = $matrix[$r][$c];
        for($k=$c;$k<=$numButtons;$k++) $matrix[$r][$k] /= $s;
        for($i=0;$i<$n;$i++) if($i!=$r && abs($matrix[$i][$c])>1e-9){
            $f = $matrix[$i][$c];
            for($k=$c;$k<=$numButtons;$k++) $matrix[$i][$k] -= $f*$matrix[$r][$k];
        }
        $pivotCol[$r++] = $c;
    }
    $rank = $r;
    for($i=$rank;$i<$n;$i++) if(abs($matrix[$i][$numButtons])>1e-9) return -1;
    $isP = array_fill(0,$numButtons,false);
    for($i=0;$i<$rank;$i++) if($pivotCol[$i]>=0) $isP[$pivotCol[$i]] = true;
    $freeVars = [];
    for($i=0;$i<$numButtons;$i++) if(!$isP[$i]) $freeVars[]=$i;
    $maxPresses = array_fill(0,$numButtons,0);
    foreach($buttons as $i=>$b){
        $m = PHP_INT_MAX;
        foreach($b as $j) if($j<$n) $m = min($m,$targets[$j]);
        $maxPresses[$i] = ($m===PHP_INT_MAX)?0:$m;
    }
    usort($freeVars,function($a,$b)use($maxPresses){return $maxPresses[$a] <=> $maxPresses[$b];});
    $best = PHP_INT_MAX;
    $freeVals = array_fill(0,count($freeVars),0);
    $enumerate = function($idx,$sum) use (&$enumerate,&$freeVals,&$freeVars,&$maxPresses,&$best,$rank,$pivotCol,$matrix,$numButtons,$targets){
        if($sum>=$best) return;
        if($idx===count($freeVars)){
            $res = array_fill(0,$numButtons,0);
            foreach($freeVars as $i=>$v) $res[$v] = $freeVals[$i];
            for($i=$rank-1;$i>=0;$i--){
                $c = $pivotCol[$i];
                if($c<0) continue;
                $v = $matrix[$i][$numButtons];
                for($k=$c+1;$k<$numButtons;$k++) $v -= $matrix[$i][$k]*$res[$k];
                $iv = (int)round($v);
                if(abs($v-$iv)>1e-6 || $iv<0 || $iv>$maxPresses[$c]) return;
                $res[$c] = $iv;
            }
            $cur = array_sum($res);
            if($cur<$best) $best=$cur;
        }else{
            $fv = $freeVars[$idx];
            for($v=0;$v<=$maxPresses[$fv];$v++){
                $freeVals[$idx]=$v;
                $enumerate($idx+1,$sum+$v);
            }
        }
    };
    $enumerate(0,0);
    return $best===PHP_INT_MAX?-1:$best;
}
$total = 0;
$lines = file('input.txt', FILE_IGNORE_NEW_LINES);
foreach($lines as $line){
    $line = trim($line);
    if($line==='') continue;
    $buttons = [];
    if(preg_match_all('/\(([^)]*)\)/',$line,$bm)){
        foreach($bm[1] as $s){
            $s = trim($s);
            if($s===''){ $buttons[] = []; continue; }
            $parts = array_map('trim',explode(',',$s));
            $arr = [];
            foreach($parts as $p) if($p!=='') $arr[] = (int)$p;
            $buttons[] = $arr;
        }
    }
    $targets = [];
    if(preg_match('/\{([^}]*)\}/',$line,$tm)){
        $parts = array_map('trim',explode(',',$tm[1]));
        foreach($parts as $p) if($p!=='') $targets[] = (int)$p;
    }
    $total += solve($buttons,$targets);
}
echo $total;
?>