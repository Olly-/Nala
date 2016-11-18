unit nala.Time;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dateutils;

const
  MSecsPerHour = MSecsPerDay div 24;
  MSecsPerMinute = MSecsPerHour div 60;

type
  TMSFormat = (
    mfDigit, // 00:00:00:00
    mfShort, // 0 days, 0 hours, 0 mins, 0 secs
    mfLong   // 0 days, 0 hours, 0 minutes and 0 seconds
  );

  TMSFormatFlag = (mffSeconds, mffDays, mffUpperCase);
  TMSFormatFlags = set of TMSFormatFlag;

procedure DecodeMilliseconds(MS: UInt64; var Days, Hours, Minutes, Seconds: UInt32);
function ConvertMilliseconds(MS: UInt64; Style: TMSFormat; Flags: TMSFormatFlags): String;

implementation

uses
  strutils;

procedure DecodeMilliseconds(MS: UInt64; var Days, Hours, Minutes, Seconds: UInt32);
begin
  Days := MS div MSecsPerDay;
  MS := MS mod MSecsPerDay;
  Hours := MS div MSecsPerHour;
  MS := MS mod MSecsPerHour;
  Minutes := MS div MSecsPerMinute;
  MS := MS mod MSecsPerMinute;
  Seconds := MS div MSecsPerSec;
end;

function ConvertMilliseconds(MS: UInt64; Style: TMSFormat; Flags: TMSFormatFlags): String;

  function Stringify(Times: array of UInt32; Strings: array of String): String;

    function Add(Int: UInt32; Str: String): String;
    begin
      if (mffUpperCase in Flags) then
        Str[1] := UpCase(Str[1])
      else
        Str[1] := LowerCase(Str[1]);

      Result := IntToStr(Int) + ' ' + Str;
      if (Int <> 1) then
        Result += 's';
    end;

  begin
    Result := '';

    // Days
    if (mffDays in Flags) then
      Result += Add(Times[0], Strings[0]) + ', ';
    // Hours
    Result += Add(Times[1], Strings[1]);
    if (mffSeconds in Flags) then
      Result += ', '
    else
      Result += ' and ';
    // Minutes
    Result += Add(Times[2], Strings[2]);
    // Seconds
    if (mffSeconds in Flags) then
      Result += ' and ' + Add(Times[3], Strings[3]);
  end;

var
  Day, Hour, Min, Sec: UInt32;
begin
  DecodeMilliseconds(MS, Day, Hour, Min, Sec);
  if (not (mffDays in Flags)) then
  begin
    Hour += Day * 24;
    Day := 0;
  end;

  case Style of
    mfDigit:
      begin
        if (mffDays in Flags) then
          Result := AddChar('0', IntToStr(Day), 2) + ':';
        Result += AddChar('0', IntToStr(Hour), 2) + ':' + AddChar('0', IntToStr(Min), 2);
        if (mffSeconds in Flags) then
          Result +=  ':' + AddChar('0', IntToStr(Sec), 2);
      end;
    mfShort: Result := Stringify([Day, Hour, Min, Sec], ['day', 'hour', 'min', 'sec']);
    mfLong: Result := Stringify([Day, Hour, Min, Sec], ['day', 'hour', 'minute', 'second']);
  end;
end;

end.

