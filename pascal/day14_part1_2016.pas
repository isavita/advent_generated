
program Day14;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

const
  CACHE_SIZE = 2048;
  S: array[0..63] of Byte = (
    7,12,17,22, 7,12,17,22, 7,12,17,22, 7,12,17,22,
    5,9,14,20, 5,9,14,20, 5,9,14,20, 5,9,14,20,
    4,11,16,23, 4,11,16,23, 4,11,16,23, 4,11,16,23,
    6,10,15,21, 6,10,15,21, 6,10,15,21, 6,10,15,21);
  K: array[0..63] of LongWord = (
    $d76aa478,$e8c7b756,$242070db,$c1bdceee,$f57c0faf,$4787c62a,
    $a8304613,$fd469501,$698098d8,$8b44f7af,$ffff5bb1,$895cd7be,
    $6b901122,$fd987193,$a679438e,$49b40821,$f61e2562,$c040b340,
    $265e5a51,$e9b6c7aa,$d62f105d,$02441453,$d8a1e681,$e7d3fbc8,
    $21e1cde6,$c33707d6,$f4d50d87,$455a14ed,$a9e3e905,$fcefa3f8,
    $676f02d9,$8d2a4c8a,$fffa3942,$8771f681,$6d9d6122,$fde5380c,
    $a4beea44,$4bdecfa9,$f6bb4b60,$bebfbc70,$289b7ec6,$eaa127fa,
    $d4ef3085,$04881d05,$d9d4d039,$e6db99e5,$1fa27cf8,$c4ac5665,
    $f4292244,$432aff97,$ab9423a7,$fc93a039,$655b59c3,$8f0ccc92,
    $ffeff47d,$85845dd1,$6fa87e4f,$fe2ce6e0,$a3014314,$4e0811a1,
    $f7537e82,$bd3af235,$2ad7d2bb,$eb86d391);

type
  TCacheEntry = record
    Index: Integer;
    Hash : string[32];
  end;

var
  Cache: array[0..CACHE_SIZE-1] of TCacheEntry;

function ROTL(x: LongWord; c: Byte): LongWord;
begin
  Result := (x shl c) or (x shr (32 - c));
end;

procedure ToHex(const Digest: array of Byte; out HexStr: string);
var
  i: Integer;
begin
  SetLength(HexStr, 32);
  for i := 0 to 15 do
    HexStr[1 + i*2] := IntToHex(Digest[i],2)[1];
  for i := 0 to 15 do
    HexStr[2 + i*2] := IntToHex(Digest[i],2)[2];
end;

procedure MD5(const Msg: array of Byte; Len: Integer; out Digest: array of Byte);
var
  H0,H1,H2,H3: LongWord;
  BitLen: UInt64;
  NewLen, Offset, i, g: Integer;
  Padded: TBytes;
  M: array[0..15] of LongWord;
  A,B,C,D,F,Temp: LongWord;
begin
  H0 := $67452301; H1 := $EFCDAB89; H2 := $98BADCFE; H3 := $10325476;
  BitLen := UInt64(Len) * 8;
  NewLen := ((Len + 8) div 64 + 1) * 64;
  SetLength(Padded, NewLen);
  Move(Msg[0], Padded[0], Len);
  Padded[Len] := $80;
  Move(BitLen, Padded[NewLen-8], 8);
  Offset := 0;
  while Offset < NewLen do
  begin
    Move(Padded[Offset], M[0], 64);
    A := H0; B := H1; C := H2; D := H3;
    for i := 0 to 63 do
    begin
      if i < 16 then begin F := (B and C) or ((not B) and D); g := i; end
      else if i < 32 then begin F := (D and B) or ((not D) and C); g := (5*i + 1) mod 16; end
      else if i < 48 then begin F := B xor C xor D; g := (3*i + 5) mod 16; end
      else begin F := C xor (B or (not D)); g := (7*i) mod 16; end;
      Temp := D;
      D := C;
      C := B;
      B := B + ROTL(A + F + K[i] + M[g], S[i]);
      A := Temp;
    end;
    H0 := H0 + A; H1 := H1 + B; H2 := H2 + C; H3 := H3 + D;
    Inc(Offset,64);
  end;
  Move(H0, Digest[0], 4);
  Move(H1, Digest[4], 4);
  Move(H2, Digest[8], 4);
  Move(H3, Digest[12],4);
end;

function GetHash(const Salt: string; Index: Integer): string;
var
  CacheIdx: Integer;
  Buf: string;
  Bytes: TBytes;
  Digest: array[0..15] of Byte;
begin
  CacheIdx := Index mod CACHE_SIZE;
  if Cache[CacheIdx].Index = Index then
    Exit(Cache[CacheIdx].Hash);
  Buf := Salt + IntToStr(Index);
  SetLength(Bytes, Length(Buf));
  Move(Buf[1], Bytes[0], Length(Buf));
  MD5(Bytes, Length(Buf), Digest);
  ToHex(Digest, Result);
  Cache[CacheIdx].Index := Index;
  Cache[CacheIdx].Hash := Result;
end;

var
  Salt: string;
  f: TextFile;
  i, Found, Index: Integer;
  Triplet: Char;
  Quint: string;
  HashStr: string;
begin
  AssignFile(f,'input.txt'); Reset(f);
  ReadLn(f,Salt); CloseFile(f);
  for i:=0 to CACHE_SIZE-1 do Cache[i].Index:=-1;
  Found:=0; Index:=0;
  while Found<64 do
  begin
    HashStr:=GetHash(Salt,Index);
    Triplet:=#0;
    for i:=1 to 30 do
      if (HashStr[i]=HashStr[i+1]) and (HashStr[i]=HashStr[i+2]) then
      begin Triplet:=HashStr[i]; Break; end;
    if Triplet<>#0 then
    begin
      Quint:=StringOfChar(Triplet,5);
      for i:=1 to 1000 do
        if Pos(Quint,GetHash(Salt,Index+i))>0 then
        begin Inc(Found); Break; end;
    end;
    Inc(Index);
  end;
  WriteLn(Index-1);
end.
