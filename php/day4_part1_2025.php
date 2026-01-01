<?php
$lines=file('input.txt',FILE_IGNORE_NEW_LINES);
$grid=[];
foreach($lines as $l){
    $l=rtrim($l,"\r\n");
    if($l==='')continue;
    $grid[]=$l;
}
$rows=count($grid);
$cols=$rows?strlen($grid[0]):0;
$dx=[-1,-1,-1,0,0,1,1,1];
$dy=[-1,0,1,-1,1,-1,0,1];
$acc=0;
for($y=0;$y<$rows;$y++){
    for($x=0;$x<$cols;$x++){
        if($grid[$y][$x]!=='@')continue;
        $cnt=0;
        for($d=0;$d<8;$d++){
            $nx=$x+$dx[$d];
            $ny=$y+$dy[$d];
            if($nx>=0&&$nx<$cols&&$ny>=0&&$ny<$rows&&$grid[$ny][$nx]==='@')$cnt++;
        }
        if($cnt<4)$acc++;
    }
}
echo $acc.PHP_EOL;
?>