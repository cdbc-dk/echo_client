

                   {**************************************$
                   $  Unit name : bc_strings.pas          $
                   $  Copyright : (C)2009-2020 cdbc.dk    $
                   $  Programmer: Benny Christensen       $
                   $  Created   : 2009-11-13 /bc          $
                   $  Updated   : 2020-03-24 /bc          $
                   $              2020.04.18 /bc          $
                   $ ************************************ $
                   $  Purpose   :                         $
                   $  One string class, which implements  $
                   $  operations on a string.             $
                   $**************************************}

unit DO_NOT_USE__bc_strings;
{$define fpc}
{$ifdef fpc}
  {$mode objfpc}{$H+}
{$endif}
interface
uses
  Classes,
  Sysutils;

type
  TStringArray = array of string;

  IStringWorkshop = interface ['{2220CA2B-FEAB-48B4-A277-44C13C9F6C31}']
		function StrCase(const S: string;Elements: array of string): ptrint;
    function StrCaseLen(const S: string;Elements: array of string;const aLength: ptrint): ptrint;
		function StrCase4(const S: string; aChoice: array of string): ptrint; { only acts on the first 4 chars }
    function Str2Bool(const S: string): boolean;
    function Bool2Str(const B: boolean): string;
    function GetFieldToken(const FieldNo: integer;const S: string;const Separator: char): string;
		function StrLowCase(const S: string): string;
		function CloneString(const aSource: string): string; { duplicate NOT reference }
		function SwapChar(const OldChar,NewChar: char;const S: string): string;
		function ShortenFilename(const aFilename: string): string;
		procedure PutString(SubStr: string;var Dest: string;Index,FieldLen: integer);
  end; { IStringWorkshop }

{ factory creates a singleton }
function StringWorkshop: IStringWorkshop;

function StrCase(const S: string;Elements: array of string): integer;
function Str2Bool(const S: string): boolean;

implementation

type
  { *** TStringWorkshop *** }

  { TStringWorkshop }

  TStringWorkshop = class(TInterfacedObject,IStringWorkshop)
  private
		function StrCase(const S: string;Elements: array of string): ptrint;
		function StrCase4(const S: string; aChoice: array of string): ptrint; { only acts on the first 4 chars }
    function Str2Bool(const S: string): boolean;
    function Bool2Str(const B: boolean): string;
    function GetFieldToken(const FieldNo: integer;const S: string;const Separator: char): string;
    function StrLowCase(const S: string): string;
		function CloneString(const aSource: string): string; { duplicate NOT reference }
		function SwapChar(const OldChar,NewChar: char;const S: string): string;
		function ShortenFilename(const aFilename: string): string;
		procedure PutString(SubStr: string;var Dest: string;Index,FieldLen: integer);
  public
    constructor Create;
    destructor Destroy; override;
  end;

{ TStringWorkshop }
(* StrCase usage....
  case StrCase(S,['+date','+hello','+data','+quit']) of
    0: // '+date'
    1: // '+hello'
    ...
  end;
  
function StrLowCase(const S: string): string;
{-------------------------------------------}

  
*)
function TStringWorkshop.StrCase(const S: string; Elements: array of string
  ): ptrint;
var Idx: integer;
begin
  Result:= -1;
  for Idx:= low(Elements) to high(Elements) do if S = Elements[Idx] then begin
    Result:= Idx;
    break;
  end;
end; { StrCase }



function TStringWorkshop.StrCase4(const S: string; aChoice: array of string): ptrint;
var Idx: ptrint;
begin
  Result:= -1;
  for Idx:= low(aChoice) to high(aChoice) do if copy(S,1,4) = aChoice[Idx] then begin
    Result:= Idx;
    break;
  end;
end; { StrCase4 }

function TStringWorkshop.Str2Bool(const S: string): boolean; { defaults to false }
begin
  case StrCase(S,['false','False','FALSE','nej','Nej','NEJ','true','True','TRUE','ja','Ja','JA']) of
    0..5: Result:= false;
    6..11: Result:= true;
    else Result:= false;
  end;
end; { Str2bool }

function TStringWorkshop.Bool2Str(const B: boolean): string;
begin
  if B then Result:= 'True' else Result:= 'False';
end; { Bool2str }

{ usage: Surname:= GetFieldToken(2,'Benny|Christensen','|') => 'Christensen'  or }
{        Firstname:= GetFieldToken(1,'Benny Christensen',' ') => 'Benny' }
function TStringWorkshop.GetFieldToken(const FieldNo: integer; const S: string;
  const Separator: char): string;
var
  B,E,C,I,Len: integer;
  InField: boolean;
begin
  Len:= system.length(S);
  if (Len > 0) and (FieldNo > 0) then begin                 { save clockcycles }
    I:= 0; C:= 1; InField:= false; B:= -1; E:= -1;            { initialization }
    while (C <= Len) do begin
      if (system.copy(S,C,1) = Separator) or
         (C = Len) then inc(I);        { check for separator and end of string }
      if (I = FieldNo-1) and not InField then begin        { 0-based by nature }
        B:= C;                             { point b to beginning of substring }
        InField:= true;               { flag field found, now look for the end }
      end;
      if (I = FieldNo) and InField then begin
        E:= C;                                   { point e to end of substring }
        break;                                  { Field found, we're done here }
      end;
      inc(C);                                               { increment cursor }
    end;                                                          { continue ? }
    if (B <> -1) and (E <> -1) then begin
      if E = Len then Result:= system.copy(S,B+1,E-B)       { special cases at }
      else if B = 1 then Result:= system.copy(S,B,E-B)     { beginning and end }
      else Result:= system.copy(S,B+1,E-B-1);
    end else Result:= '';                       { return empty string on error }
  end else Result:= '';           { if fed an empty string return it untouched }
end; { getfieldtoken }

function TStringWorkshop.StrLowCase(const S: string): string;
var
  Ch: char;
  L: integer;
  Source, Dest: pchar;
begin { StrLowCase }
  L:= length(S);
  SetLength(Result,L);
  Source:= pointer(S);
  Dest:= pointer(Result);
  while L <> 0 do
  begin
    Ch:= Source^;
    if (Ch >= 'A') and (Ch <= 'Z') then Inc(Ch, 32) else
    if Ch = 'Æ' then Ch:= 'æ' else if Ch = 'Ø' then Ch:= 'ø' else
    if Ch = 'Å' then Ch:= 'å';
    Dest^:= Ch;
    inc(Source);
    inc(Dest);
    dec(L);
  end;
end;  { StrLowCase }

{ copies a string (duplicate) not by reference }
function TStringWorkshop.CloneString(const aSource: string): string;
var Len: ptruint;
begin
  Result:= ''; Len:= Length(aSource);
  if Len > 0 then begin
    SetLength(Result,Len);
    Move(aSource[1],Result[1],Len);
  end;
end; { CloneString }

function TStringWorkshop.SwapChar(const OldChar, NewChar: char; const S: string): string;
var
  Ch: Char;
  L: integer;
  Source,Dest: pansichar;
begin { SwapChar }
  L:= length(S);
  SetLength(Result,L);
  Source:= pointer(S);
  Dest:= pointer(Result);
  while L <> 0 do begin
    Ch := Source^;
    if Ch = OldChar then Ch:= NewChar;
    Dest^ := Ch;
    inc(Source);
    inc(Dest);
    dec(L);
  end;
end;  { SwapChar }
{ shortens a filename by cutting out the middle piece }
function TStringWorkshop.ShortenFilename(const aFilename: string): string;
var Len,Q1,Q4,Lq,Rest: cardinal;
begin
  Len:= Length(aFilename);
  Lq:= Len div 4;
  Rest:= (Len - (Lq * 4)) + 1; { 0-based math on 1-based string }
  Q1:= 1; Q4:= Lq * 3;
  Result:= copy(aFilename,Q1,Lq) + '\...\' + copy(aFilename,Q4,Lq+Rest);
end; { ShortenFilename }

{ if called with FieldLen = -1, no spacepadding is performed }
{ if length(substr) < Fieldlen, SubStr is right justified }
procedure TStringWorkshop.PutString(SubStr: string;
                                    var Dest: string;
                                    Index,
                                    FieldLen: integer);
var
  SubLen,I: integer;
  C: char; 
begin
  if SubStr <> '' then begin
    if Dest = '' then Dest:= Dest+SubStr else begin // todo calculate and set length
      SubLen:= length(SubStr);
      if Index > length(Dest) then setlength(Dest,Index+SubLen);
      if ((FieldLen = -1) or (SubLen = FieldLen)) then      // no padding needed
        for I:= index to Index+SubLen do begin
          C:= SubStr[(I-Index)+1];
          if C = #0 then C:= ' '; // hmmm...
          Dest[I]:= C;
        end
      else begin                                          // spacepadding needed
        while length(SubStr) < FieldLen do SubStr:= ' '+SubStr;
        for I:= index to Index+FieldLen-1 do begin
          C:= SubStr[(I-Index)+1];
          if C = #0 then C:= ' '; // hmmm... ok, seems to work... what a hack!
          Dest[I]:= C;
        end;
      end;
    end;
  end;
end; { PutString }

constructor TStringWorkshop.Create;
begin
  // for now, do nothing
end;

destructor TStringWorkshop.Destroy;
begin
  inherited Destroy;
end;

  { TStringWorkshop }

var
  S_Workshop: IStringWorkshop = nil;


function StringWorkshop: IStringWorkshop;
begin
  if not assigned(S_Workshop) then S_Workshop:= TStringWorkshop.Create;
  Result:= S_Workshop;
end;


(* StrCase usage....
  case StrCase(S,['+date','+hello','+data','+quit']) of
    0: // '+date'
    1: // '+hello'
    ...
  end;
*)
function StrCase(const S: string;Elements: array of string): integer;
var Idx: integer;
begin
  Result:= -1;
  for Idx:= low(Elements) to high(Elements) do if S = Elements[Idx] then begin
    Result:= Idx;
    break;
  end;
end; { StrCase }

(* returns boolean value of danish yes and no strings, defaults to false *)
function Str2Bool(const S: string): boolean;
begin
  case StrCase(S,['nej','Nej','NEJ','ja','Ja','JA']) of
    0..2: Result:= false;
    3..5: Result:= true;
    else Result:= false;
  end;
end;



initialization
  S_Workshop:= nil;
finalization
  S_Workshop:= nil;

end.

