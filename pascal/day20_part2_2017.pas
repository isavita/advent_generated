program ParticleSimulation;
uses SysUtils;
type TPart=record p,v,a:array[1..3] of Int64; alive:boolean; end;
var parts:array of TPart;
    l,numStr: string;
    tmp: array[1..9] of Int64;
    N,I,J,T,pCount,numCount: Integer;
    ch: Char;
begin
  Assign(Input,'input.txt'); Reset(Input);
  N:=0;
  while not eof do begin
    ReadLn(l);
    numCount:=0; numStr:='';
    for I:=1 to Length(l) do begin
      ch:=l[I];
      if (ch in ['0'..'9','-']) then numStr+=ch
      else if numStr<>'' then begin
        Inc(numCount);
        tmp[numCount]:=StrToInt64(numStr);
        numStr:='';
      end;
    end;
    if numStr<>'' then begin
      Inc(numCount);
      tmp[numCount]:=StrToInt64(numStr);
    end;
    SetLength(parts,N+1);
    with parts[N] do begin
      for I:=1 to 3 do begin
        p[I]:=tmp[I];
        v[I]:=tmp[I+3];
        a[I]:=tmp[I+6];
      end;
      alive:=True;
    end;
    Inc(N);
  end;
  for T:=1 to 1000 do begin
    for I:=0 to N-1 do
      if parts[I].alive then
        for J:=1 to 3 do begin
          parts[I].v[J]:=parts[I].v[J]+parts[I].a[J];
          parts[I].p[J]:=parts[I].p[J]+parts[I].v[J];
        end;
    for I:=0 to N-2 do
      if parts[I].alive then
        for J:=I+1 to N-1 do
          if parts[J].alive and
             (parts[I].p[1]=parts[J].p[1]) and
             (parts[I].p[2]=parts[J].p[2]) and
             (parts[I].p[3]=parts[J].p[3]) then begin
            parts[I].alive:=False;
            parts[J].alive:=False;
          end;
  end;
  pCount:=0;
  for I:=0 to N-1 do
    if parts[I].alive then Inc(pCount);
  WriteLn(pCount);
end.
