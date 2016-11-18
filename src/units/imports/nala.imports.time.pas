unit nala.Imports.Time;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lptypes, dateutils,
  nala.LapeCompiler;

procedure Import_Time(Compiler: TLPCompiler);

implementation

uses
  LazUTF8SysUtils,
  nala.Time;

type
  PMSFormat = ^TMSFormat;
  PMSFormatFlags = ^TMSFormatFlags;

procedure Lape_ConvertMilliseconds(Params: PParamArray; const Result: Pointer);
begin
  PlpString(Result)^ := ConvertMilliseconds(PUInt64(Params^[0])^, PMSFormat(Params^[1])^, PMSFormatFlags(Params^[2])^);
end;

procedure Lape_DecodeMilliseconds(Params: PParamArray);
begin
  DecodeMilliseconds(PUInt64(Params^[0])^, PUInt32(Params^[1])^, PUInt32(Params^[2])^, PUInt32(Params^[3])^, PUInt32(Params^[4])^);
end;

procedure Lape_DayOf(Params: PParamArray; const Result: Pointer);
begin
  PUInt16(Result)^ := DayOf(PDateTime(Params^[0])^);
end;

procedure Lape_HourOf(Params: PParamArray; const Result: Pointer);
begin
  PUInt16(Result)^ := HourOf(PDateTime(Params^[0])^);
end;

procedure Lape_MinuteOf(Params: PParamArray; const Result: Pointer);
begin
  PUInt16(Result)^ := MinuteOf(PDateTime(Params^[0])^);
end;

procedure Lape_SecondOf(Params: PParamArray; const Result: Pointer);
begin
  PUInt16(Result)^ := SecondOf(PDateTime(Params^[0])^);
end;

procedure Lape_MilliSecondOf(Params: PParamArray; const Result: Pointer);
begin
  PUInt16(Result)^ := MilliSecondOf(PDateTime(Params^[0])^);
end;

procedure Lape_DaysBetween(Params: PParamArray; const Result: Pointer);
begin
  PInteger(Result)^ := DaysBetween(PDateTime(Params^[0])^, PDateTime(Params^[1])^);
end;

procedure Lape_HoursBetween(Params: PParamArray; const Result: Pointer);
begin
  PInt64(Result)^ := HoursBetween(PDateTime(Params^[0])^, PDateTime(Params^[1])^);
end;

procedure Lape_MinutesBetween(Params: PParamArray; const Result: Pointer);
begin
  PInt64(Result)^ := MinutesBetween(PDateTime(Params^[0])^, PDateTime(Params^[1])^);
end;

procedure Lape_SecondsBetween(Params: PParamArray; const Result: Pointer);
begin
  PInt64(Result)^ := SecondsBetween(PDateTime(Params^[0])^, PDateTime(Params^[1])^);
end;

procedure Lape_MilliSecondsBetween(Params: PParamArray; const Result: Pointer);
begin
  PInt64(Result)^ := MilliSecondsBetween(PDateTime(Params^[0])^, PDateTime(Params^[1])^);
end;

procedure Lape_DateTimeToUnix(Params: PParamArray; const Result: Pointer);
begin
  PInt64(Result)^ := DateTimeToUnix(PDateTime(Params^[0])^);
end;

procedure Lape_UnixToDateTime(Params: PParamArray; const Result: Pointer);
begin
  PDateTime(Result)^ := UnixToDateTime(PInt64(Params^[0])^);
end;

procedure Lape_UnixTime(Params: PParamArray; const Result: Pointer);
begin
  PInt64(Result)^ := DateTimeToUnix(NowUTC());
end;

procedure Lape_NowUTC(Params: PParamArray; const Result: Pointer);
begin
  PDateTime(Result)^ := NowUTC();
end;

procedure Import_Time(Compiler: TLPCompiler);
begin
  with Compiler do
  begin
    Dump.Section := 'Time';

    addGlobalConst(MSecsPerHour, 'MSecsPerHour');
    addGlobalConst(MSecsPerMinute, 'MSecsPerMinute');

    addGlobalType('(mfDigit, mfShort, mfLong)', 'TMSFormat');
    addGlobalType('(mffSeconds, mffDays, mffUpperCase)', 'TMSFormatFlag');
    addGlobalType('set of TMSFormatFlag', 'TMSFormatFlags');

    addGlobalFunc('procedure DecodeMilliseconds(MS: UInt64; var Days, Hours, Minutes, Seconds: UInt32);', '',  @Lape_DecodeMilliseconds);
    addGlobalFunc('function ConvertMilliseconds(MS: UInt64; Style: TMSFormat; Flags: TMSFormatFlags): String;', '',  @Lape_ConvertMilliseconds);
    addGlobalFunc('function DayOf(const AValue: TDateTime): UInt16;', '',  @Lape_DayOf);
    addGlobalFunc('function HourOf(const AValue: TDateTime): UInt16;', '',  @Lape_HourOf);
    addGlobalFunc('function MinuteOf(const AValue: TDateTime): UInt16;', '',  @Lape_MinuteOf);
    addGlobalFunc('function SecondOf(const AValue: TDateTime): UInt16;', '',  @Lape_SecondOf);
    addGlobalFunc('function MilliSecondOf(const AValue: TDateTime): UInt16;', '',  @Lape_MilliSecondOf);
    addGlobalFunc('function DaysBetween(const ANow, AThen: TDateTime): Integer;', '',  @Lape_DaysBetween);
    addGlobalFunc('function HoursBetween(const ANow, AThen: TDateTime): Int64;', '',  @Lape_HoursBetween);
    addGlobalFunc('function MinutesBetween(const ANow, AThen: TDateTime): Int64;', '',  @Lape_MinutesBetween);
    addGlobalFunc('function SecondsBetween(const ANow, AThen: TDateTime): Int64;', '',  @Lape_SecondsBetween);
    addGlobalFunc('function MilliSecondsBetween(const ANow, AThen: TDateTime): Int64;', '',  @Lape_MilliSecondsBetween);
    addGlobalFunc('function DateTimeToUnix(const AValue: TDateTime): Int64;', '',  @Lape_DateTimeToUnix);
    addGlobalFunc('function UnixToDateTime(const AValue: Int64): TDateTime;', '',  @Lape_UnixToDateTime);
    addGlobalFunc('function UnixTime: Int64;', '', @Lape_UnixTime);
    addGlobalFunc('function NowUTC: TDateTime', '',  @Lape_NowUTC);
  end;
end;

end.

