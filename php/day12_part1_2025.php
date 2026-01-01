
<?php
function normalize($piece){
    if(!$piece) return $piece;
    $minr=PHP_INT_MAX;$minc=PHP_INT_MAX;
    foreach($piece as $pt){
        if($pt[0]<$minr)$minr=$pt[0];
        if($pt[1]<$minc)$minc=$pt[1];
    }
    $norm=[];
    foreach($piece as $pt){
        $norm[]=[ $pt[0]-$minr, $pt[1]-$minc ];
    }
    usort($norm,function($a,$b){
        return $a[0]==$b[0] ? $a[1]<$b[1]?-1:1 : ($a[0]<$b[0]?-1:1);
    });
    return $norm;
}
function rotateP($piece){
    $res=[];
    foreach($piece as $pt){
        $res[]=[ $pt[1], -$pt[0] ];
    }
    return $res;
}
function flipP($piece){
    $res=[];
    foreach($piece as $pt){
        $res[]=[ $pt[0], -$pt[1] ];
    }
    return $res;
}
function pieceEqual($a,$b){
    if(count($a)!=count($b))return false;
    for($i=0;$i<count($a);$i++){
        if($a[$i][0]!=$b[$i][0]||$a[$i][1]!=$b[$i][1])return false;
    }
    return true;
}
function generateVariations($base,&$cnt){
    $uniq=[];
    $curr=$base;
    for($i=0;$i<4;$i++){
        $n=normalize($curr);
        $ok=true;
        foreach($uniq as $u) if(pieceEqual($u,$n)){ $ok=false; break; }
        if($ok)$uniq[]=$n;
        $f=flipP($curr);
        $nf=normalize($f);
        $ok=true;
        foreach($uniq as $u) if(pieceEqual($u,$nf)){ $ok=false; break; }
        if($ok)$uniq[]=$nf;
        $curr=rotateP($curr);
    }
    $cnt=count($uniq);
    return $uniq;
}
function canPlace($rows,$cols,$grid,$piece,$r,$c){
    foreach($piece as $pt){
        $nr=$r+$pt[0];$nc=$c+$pt[1];
        if($nr<0||$nr>=$rows||$nc<0||$nc>=$cols)return false;
        if($grid[$nr*$cols+$nc])return false;
    }
    return true;
}
function place(&$grid,$cols,$piece,$r,$c,$val){
    foreach($piece as $pt){
        $grid[($r+$pt[0])*$cols+($c+$pt[1])]=$val;
    }
}
function checkIslands($rows,$cols,$grid,$counts,$arrSize,$slackIdx,$shapes){
    $minReal=PHP_INT_MAX;$hasReal=false;
    for($i=0;$i<$arrSize;$i++)if($i!=$slackIdx && $counts[$i]>0){
        $hasReal=true;
        if($shapes[$i] && count($shapes[$i])<$minReal)$minReal=count($shapes[$i]);
    }
    if(!$hasReal)return true;
    $avail=$counts[$slackIdx];
    $vis=array_fill(0,$rows*$cols,false);
    $q=[];
    for($i=0;$i<$rows*$cols;$i++){
        if(!$grid[$i] && !$vis[$i]){
            $qs=0;$q[0]=$i;$qe=1;$vis[$i]=true;$size=0;
            while($qs<$qe){
                $cur=$q[$qs++];
                $size++;
                $r=intdiv($cur,$cols);$c=$cur%$cols;
                $nbr=[[$r-1,$c],[$r+1,$c],[$r,$c-1],[$r,$c+1]];
                foreach($nbr as $n){
                    [$nr,$nc]=$n;
                    if($nr<0||$nr>=$rows||$nc<0||$nc>=$cols)continue;
                    $idx=$nr*$cols+$nc;
                    if(!$grid[$idx] && !$vis[$idx]){
                        $vis[$idx]=true;$q[$qe++]=$idx;
                    }
                }
            }
            if($size<$minReal){
                if($avail>=$size)$avail-=$size;
                else return false;
            }
        }
    }
    return true;
}
function solveRec($rows,$cols,&$grid,&$counts,$arrSize,$ids,$varCounts,$variations,$slackIdx,$shapes){
    $empty=-1;
    $total=$rows*$cols;
    for($i=0;$i<$total;$i++)if(!$grid[$i]){$empty=$i;break;}
    if($empty==-1)return true;
    $r=intdiv($empty,$cols);$c=$empty%$cols;
    if(!checkIslands($rows,$cols,$grid,$counts,$arrSize,$slackIdx,$shapes))return false;
    foreach($ids as $id){
        if($counts[$id]==0)continue;
        $counts[$id]--;
        for($v=0;$v<$varCounts[$id];$v++){
            $p=$variations[$id][$v];
            if(canPlace($rows,$cols,$grid,$p,$r,$c)){
                place($grid,$cols,$p,$r,$c,1);
                if(solveRec($rows,$cols,$grid,$counts,$arrSize,$ids,$varCounts,$variations,$slackIdx,$shapes))return true;
                place($grid,$cols,$p,$r,$c,0);
            }
        }
        $counts[$id]++;
    }
    return false;
}
$lines=file('input.txt',FILE_IGNORE_NEW_LINES);
$maxId=-1;
foreach($lines as $ln){
    $s=trim($ln);
    if($s && substr($s,-1)===':'){
        $id=intval(rtrim($s,':'));
        if($id>$maxId)$maxId=$id;
    }
}
$arrSize=$maxId+2;$slackIdx=$maxId+1;
$shapes=array_fill(0,$arrSize,null);
$parsingShapes=true;$curId=-1;$curShape=[];
$regionLines=[];
foreach($lines as $ln){
    $s=trim($ln);
    if($s==='')continue;
    if(strpos($s,'x')!==false && strpos($s,':')!==false)$parsingShapes=false;
    if($parsingShapes){
        if(substr($s,-1)===':'){
            if($curId!=-1 && $curShape){
                $pts=[];
                foreach($curShape as $r=>$row){
                    for($c=0;$c<strlen($row);$c++)if($row[$c]==='#'){
                        $pts[]=[ $r,$c ];
                    }
                }
                $shapes[$curId]=normalize($pts);
            }
            $curId=intval(rtrim($s,':'));
            $curShape=[];
        }else{
            $curShape[]=$s;
        }
    }else{
        $regionLines[]=$s;
    }
}
if($curId!=-1 && $curShape){
    $pts=[];
    foreach($curShape as $r=>$row){
        for($c=0;$c<strlen($row);$c++)if($row[$c]==='#'){
            $pts[]=[ $r,$c ];
        }
    }
    $shapes[$curId]=normalize($pts);
}
$shapes[$slackIdx]=[[0,0]];
$variations=$varCounts=array_fill(0,$arrSize,null);
for($i=0;$i<$arrSize;$i++){
    if($shapes[$i]===null)continue;
    $variations[$i]=generateVariations($shapes[$i],$varCounts[$i]);
}
$solved=0;
foreach($regionLines as $ln){
    $parts=explode(':',$ln,2);
    if(count($parts)!=2)continue;
    $dims=trim($parts[0]);$cntStr=trim($parts[1]);
    if(sscanf($dims,"%dx%d",$wx,$h)!==2)continue;
    $gridSize=$wx*$h;
    $pieceCounts=array_fill(0,$arrSize,0);
    $totalArea=0;$idx=0;
    foreach(preg_split('/\s+/', $cntStr) as $tok){
        if($tok==='')continue;
        $c=intval($tok);
        if($c>0 && $idx<$arrSize-1){
            $pieceCounts[$idx]=$c;
            $totalArea+=$c*($shapes[$idx]===null?0:count($shapes[$idx]));
        }
        $idx++;
    }
    if($totalArea>$gridSize)continue;
    $slack=$gridSize-$totalArea;
    if($slack>0)$pieceCounts[$slackIdx]=$slack;
    $ids=[];
    for($i=0;$i<$arrSize;$i++)if($pieceCounts[$i]>0)$ids[]=$i;
    usort($ids,function($a,$b) use($shapes){
        $na=$shapes[$a]===null?0:count($shapes[$a]);
        $nb=$shapes[$b]===null?0:count($shapes[$b]);
        return $nb<=>$na;
    });
    $grid=array_fill(0,$gridSize,0);
    if(solveRec($h,$wx,$grid,$pieceCounts,$arrSize,$ids,$varCounts,$variations,$slackIdx,$shapes))$solved++;
}
echo "Number of regions that fit all presents: $solved\n";
?>
