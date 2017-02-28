unit misc;
interface

function GetRandomFloat(Min, Max: Currency): Currency;

implementation

uses math;

function GetRandomFloat(Min, Max: Currency): Currency;
  var
     Frac: Currency;
     Whole: LongInt;

  begin
    Whole := Random(Trunc(Max) + Trunc(Min)) + 1;
    if Whole > Trunc(Max) div 2 then
      Whole:= Whole - Trunc(Min)
    else
      Whole:= Whole + Trunc(Min);

    Frac := simpleroundto(Random, -2);
    result := Whole + Frac;
    if result < 0 then result:= 0;
  end;

end.