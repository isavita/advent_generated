
{if($1=="forward") {h+=$2; d+=aim*$2} else if($1=="down") aim+=$2; else aim-=$2}
END {print h*d}
