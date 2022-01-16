(* ::Package:: *)

(* ::Chapter:: *)
(*Import and load libraries*)


(* Mathematica Package *)

(* Title:     Interface for cDateFunctions LibraryLink *)
(* Author:    Benjamin Izadpanah *)
(* Copyright: Benjamin Izadpanah *)
(* Contact:   ben.izd@outlook.com *)

(* Summary:   This is an interface for cDateFunctions DLL, which is required to use this file.
			  This package contains equivalent functions to Wolfram-Language built-in date-related functions like AbsoluteTime,UnixTime,CalendarConvert,...
			  which is written in low-level language (Rust) to speed up the calculations. 
			  
			  Three calendars are supported:
			  - Gregorian
			  - ArithmeticPersian
			  - Islamic
			  - AstronomicalPersian (delayed because of licensing issues)
			  
			  2 Paths should be provided for running this file:
			  - Path to DLL that contains main functions (compiled from Rust)
			  - Path to Interface DLL to connect to the main DLL 
			  Link to access both files:  https://github.com/ben-izd/cDateFunctions
			  *)
		
(* Context:   cDateFunctions` *)			  	  
(* Context Conflict: If you ran the cDateFunctions package (written in plain Wolfram-Language), cLeapYearQ will be overriden to use kernel-level implementation. *)
(* Start Date:         2021-11 *)
(* Last Modified Date: 2021-12 *)

(* 30 main built-in functions used to build main kernel functions (not all the functions listed) *)

(* {"Append","Begin","BeginPackage","Block","ClearAll","DateObject","Dot",
"End","EndPackage","First","Floor","If","Infinity","Last","LibraryFunctionLoad",
"LibraryFunctionUnload","LibraryLoad","Message","Mod","N","Names","Options",
"OptionsPattern","OptionValue","PadRight","Quiet","QuotientRemainder",
"Repeated","Switch","SyntaxInformation","FileExistsQ"} *)

(* These functions used only for calendrical view functions *)

(* {"ArrayPad","Catenate","Except","Join","Nothing","Optional","PadLeft","Partition",
"Range","Replace","Rest","RotateRight","StringRiffle","TableForm","ToString"} *)

If[!FileExistsQ[cDateFunctions`LibraryLink`Path`DLL],Print["Variable \"cDateFunctions`LibraryLink`Path`DLL\" is not set to a valid file. Check \"https://github.com/ben-izd/cDateFunctions\" for more information."];Abort[]];
If[!FileExistsQ[cDateFunctions`LibraryLink`Path`InterfaceDLL],Print["Variable \"cDateFunctions`LibraryLink`Path`InterfaceDLL\" is not set to a valid file. Check \"https://github.com/ben-izd/cDateFunctions\" for more information."];Abort[]];

Begin["cDateFunctions`LibraryLink`"];
ClearAll[cLinkLoad,cLinkUnload];

Load[]:=Block[{cLibPath=cDateFunctions`LibraryLink`Path`InterfaceDLL},
LibraryLoad[cDateFunctions`LibraryLink`Path`DLL];

ClearAll/@Names["cDateFunctions`LibraryLink`*Link"];

DateListNowLink=LibraryFunctionLoad[cLibPath,"date_list_now_link",{},{Integer,1}];
DateListToDateListLink=LibraryFunctionLoad[cLibPath,"date_list_to_date_list_link",{{Integer,1}},{Integer,1}];
DateListToDateListExtraLink=LibraryFunctionLoad[cLibPath,"date_list_to_date_list_extra_link",{{Integer,1},Integer},{Integer,1}];
DateListToSecondLink=LibraryFunctionLoad[cLibPath,"date_list_to_second_link",{{Integer,1}},Integer];
SecondToDateListLink=LibraryFunctionLoad[cLibPath,"second_to_date_list_link",{Integer},{Integer,1}];

AbsoluteNowLink=LibraryFunctionLoad[cLibPath,"absolute_time_now_link",{},Real];

UnixTimeNowLink=LibraryFunctionLoad[cLibPath,"unix_time_now_link",{},Real];
UnixDateListToSecondLink=LibraryFunctionLoad[cLibPath,"unix_date_list_to_second_link",{{Integer,1}},Integer];
UnixSecondToDateListLink=LibraryFunctionLoad[cLibPath,"unix_second_to_date_list_link",{Integer},{Integer,1}];

JulianTimeNowLink=LibraryFunctionLoad[cLibPath,"julian_time_now_link",{},Real];
JulianSecondToDateListLink=LibraryFunctionLoad[cLibPath,"julian_second_to_date_list_link",{Integer},{Integer,1}];
JulianDateListToSecondLink=LibraryFunctionLoad[cLibPath,"julian_date_list_to_second_link",{{Integer,1}},Integer];

AbsoluteOnlySecondLink=LibraryFunctionLoad[cLibPath,"absolute_time_only_second_link",{},Real];

HolidayQLink=LibraryFunctionLoad[cLibPath,"is_holiday_with_correction_link",{{Integer,1}},True|False];

GregorianIsLeapYearLink=LibraryFunctionLoad[cLibPath,"gregorian_is_leap_year_link",{Integer},True|False];

ArithmeticPersianSecondToDateListLink=LibraryFunctionLoad[cLibPath,"arithmetic_persian_second_to_date_list_link",{Integer,Integer},{Integer,1}];
ArithmeticPersianDateListToSecondLink=LibraryFunctionLoad[cLibPath,"arithmetic_persian_date_list_to_second_link",{Integer,{Integer,1}},Integer];
ArithmeticPersianIsLeapYearLink=LibraryFunctionLoad[cLibPath,"arithmetic_persian_is_leap_year_link",{Integer,Integer},True|False];

(*AstronomicalPersianSecondToDateListLink=LibraryFunctionLoad[cLibPath,"astronomical_persian_second_to_date_list_link",{Integer},{Integer,1}];
AstronomicalPersianDateListToSecondLink=LibraryFunctionLoad[cLibPath,"astronomical_persian_date_list_to_second_link",{{Integer,1}},Integer];
AstronomicalPersianIsLeapYearLink=LibraryFunctionLoad[cLibPath,"astronomical_persian_is_leap_year_link",{Integer},True|False]; *)

IslamicSecondToDateListLink=LibraryFunctionLoad[cLibPath,"islamic_second_to_date_list_link",{Integer,Integer},{Integer,1}];
IslamicDateListToSecondLink=LibraryFunctionLoad[cLibPath,"islamic_date_list_to_second_link",{Integer,{Integer,1}},Integer];
IslamicIsLeapYearLink=LibraryFunctionLoad[cLibPath,"islamic_is_leap_year_link",{Integer,Integer},True|False];

GregorianMonthViewLink=LibraryFunctionLoad[cLibPath,"gregorian_month_view_link",{Integer},{Integer,1}];
GregorianYearViewLink=LibraryFunctionLoad[cLibPath,"gregorian_year_view_link",{Integer,Integer},{Integer,1}];

ArithmeticPersianMonthViewLink=LibraryFunctionLoad[cLibPath,"arithmetic_persian_month_view_link",{Integer,Integer},{Integer,1}];
ArithmeticPersianYearViewLink=LibraryFunctionLoad[cLibPath,"arithmetic_persian_year_view_link",{Integer,Integer,Integer},{Integer,1}];

(*AstronomicalPersianMonthViewLink=LibraryFunctionLoad[cLibPath,"astronomical_persian_month_view_link",{Integer},{Integer,1}];
AstronomicalPersianYearViewLink=LibraryFunctionLoad[cLibPath,"astronomical_persian_year_view_link",{Integer,Integer},{Integer,1}];*)

IslamicMonthViewLink=LibraryFunctionLoad[cLibPath,"islamic_month_view_link",{Integer,Integer},{Integer,1}];
IslamicYearViewLink=LibraryFunctionLoad[cLibPath,"islamic_year_view_link",{Integer,Integer,Integer},{Integer,1}];

];
Load[];

Unload[]:=Block[{},
Quiet@LibraryUnload[cDateFunctions`LibraryLink`Path`DLL];

Quiet[LibraryFunctionUnload/@Symbol/@Names["cDateFunctions`LibraryLink`*Link"]];

];
End[];


(* ::Chapter:: *)
(*Define the interface*)


BeginPackage["cDateFunctions`"];

ClearAll[cLeapYearQ,cDateList,cAbsoluteTime,cDateFunctions`Private`ToDateListFormat,cDateFunctions`Private`GetCalendarType,cHolidayQ2,cUnixTime,cFromUnixTime,cFromAbsoluteTime,cJulianDate,cFromJulianDate,cCalendarConvert,cMonthView,cPartialView];

Begin["`Private`"];
(*no type annotation - used only internally*)
cDateFunctions`Private`ToDateListFormat[{year_,month_:1,day_:1,hour_:0,minute_:0,second_:0.0}]:={year,month,day,hour,minute,N@second}
SyntaxInformation[cUnixTime]={"ArgumentsPattern"->{_.,OptionsPattern[]}};
Options[cUnixTime]={TimeZone:>$TimeZone};

cUnixTime[OptionsPattern[]]:=cDateFunctions`LibraryLink`UnixTimeNowLink[]+(OptionValue[TimeZone]-$TimeZone)*3600.(*-OptionValue[TimeZone]*3600.*)

cUnixTime[date:_Integer|_Real,OptionsPattern[]]:=date-2208988800-OptionValue[TimeZone]*3600.

cUnixTime[date:{Repeated[_Integer,{1,5}]},OptionsPattern[]]:=cDateFunctions`LibraryLink`UnixDateListToSecondLink[date]-OptionValue[TimeZone]*3600.

cUnixTime[{date:Repeated[_Integer,{5}],second:_Integer|_Real},OptionsPattern[]]:=cDateFunctions`LibraryLink`UnixDateListToSecondLink[{date}]+second-OptionValue[TimeZone]*3600.

cUnixTime[DateObject[date_,_]]:=cUnixTime[date,TimeZone->$TimeZone]
cUnixTime[DateObject[date_,_,"Gregorian",offset_:$TimeZone]]:=cUnixTime[date,TimeZone->offset]
cUnixTime[date_DateObject]:=cUnixTime[cAbsoluteTime[date]]

Options[cFromUnixTime]={TimeZone:>$TimeZone};
cFromUnixTime[date:_Integer|_Real,OptionsPattern[]]:=Block[{temp=date+OptionValue[TimeZone]*3600.},DateObject[Append[cDateFunctions`LibraryLink`UnixSecondToDateListLink[Floor@temp],Mod[temp,60]],"Instant","Gregorian",OptionValue[TimeZone]]]

Options[cJulianDate]={TimeZone:>$TimeZone};
cJulianDate[]:=cDateFunctions`LibraryLink`JulianTimeNowLink[]

cJulianDate[date:_Integer|_Real,OptionsPattern[]]:=(date+208657771200-OptionValue[TimeZone]*3600.)/86400.

cJulianDate[date:{Repeated[_Integer,{1,5}]},OptionsPattern[]]:=cDateFunctions`LibraryLink`JulianDateListToSecondLink[date]/86400.-OptionValue[TimeZone]/24
cJulianDate[{date:Repeated[_Integer,{5}],second:_Integer|_Real},OptionsPattern[]]:=(cDateFunctions`LibraryLink`JulianDateListToSecondLink[{date}]+second)/86400.-OptionValue[TimeZone]/24


cJulianDate[DateObject[date_,_]]:=cJulianDate[date,TimeZone->$TimeZone]
cJulianDate[DateObject[date_,_,"Gregorian",timeZone_:$TimeZone]]:=cJulianDate[date,TimeZone->timeZone]
cJulianDate[date_DateObject]:=cJulianDate[cAbsoluteTime[date]]

(*cFromJulianDate[date:_Integer|_Real]:=DateObject[Append[cDateFunctions`Private`cJulianSecondToDateListLink[date+$TimeZone/24.],cDateFunctions`Private`cGetJulianSecondLink[date+$TimeZone/24.]],"Instant","Gregorian",$TimeZone]*)
Options[cFromJulianDate]={TimeZone:>$TimeZone};
cFromJulianDate[date:_Integer|_Real,OptionsPattern[]]:=Block[{temp=date*86400.+OptionValue[TimeZone]*3600.-208657771200},DateObject[Append[cDateFunctions`LibraryLink`SecondToDateListLink[Floor@temp],Mod[temp,60.]],"Instant","Gregorian",OptionValue[TimeZone]]]

cFromAbsoluteTime[date:_Integer|_Real]:=DateObject[Append[cDateFunctions`LibraryLink`SecondToDateListLink[Floor@date],Mod[date,60]],"Instant","Gregorian",$TimeZone]

cDateList::UnsupportedCalendar="Calendar type \"`1`\" is not supported.";
Options[cDateList]={Method->Automatic,TimeZone:>$TimeZone};

cDateList[OptionsPattern[]]:=Block[{temp=cDateFunctions`LibraryLink`AbsoluteNowLink[]+OptionValue[TimeZone]*3600.},Append[cDateFunctions`LibraryLink`SecondToDateListLink[Floor@temp],Mod[temp,60]]]
(*Append[cDateFunctions`LibraryLink`DateListNowLink[],cDateFunctions`LibraryLink`AbsoluteOnlySecondLink[]]*)

cDateList[date:_Integer|_Real,OptionsPattern[]]:=Block[{temp=date+(OptionValue[TimeZone]-$TimeZone)*3600.},Append[cDateFunctions`LibraryLink`SecondToDateListLink[Floor@temp],Mod[temp,60]]]

cDateList[date:{Repeated[_Integer,{1,5}]},OptionsPattern[]]:=Block[{temp=QuotientRemainder[(OptionValue[TimeZone]-$TimeZone)*3600.,60]},Append[cDateFunctions`LibraryLink`DateListToDateListExtraLink[date,First@temp],Last@temp]]
(*Append[cDateFunctions`LibraryLink`DateListToDateListLink[date],0]*)

cDateList[{date:Repeated[_Integer,{5}],second:_Integer|_Real},OptionsPattern[]]:=Block[{temp=QuotientRemainder[second+(OptionValue[TimeZone]-$TimeZone)*3600,60]},
Append[cDateFunctions`LibraryLink`DateListToDateListExtraLink[{date},First@temp],Last@temp]]

cDateList[DateObject[date_,_],OptionsPattern[]]:=cDateFunctions`Gregorian`SecondToDateList[cDateFunctions`Gregorian`DateListToSecond[date]+(OptionValue[TimeZone]-$TimeZone)*3600.]
cDateList[date:DateObject[_,_,type_,timeZone_],OptionsPattern[]]:=Switch[type,
"Gregorian",cDateFunctions`Gregorian`SecondToDateList[cDateFunctions`Gregorian`DateListToSecond[First@date]+($TimeZone-timeZone)*3600.],
"ArithmeticPersian"(*|"AstronomicalPersian"*)|"Islamic",cCalendarConvert[cAbsoluteTime[First@date,CalendarType->type]+($TimeZone-timeZone)*3600.,"Gregorian",Method->OptionValue[Method]],
_,Message[cDateList::UnsupportedCalendar,type]
]
cDateList[date:DateObject[_,_,type_],OptionsPattern[]]:=Switch[type,
"Gregorian",cDateFunctions`Gregorian`SecondToDateList[cDateFunctions`Gregorian`DateListToSecond[First@date]+($TimeZone-OptionValue[TimeZone])*3600.],
"ArithmeticPersian"(*|"AstronomicalPersian"*)|"Islamic",cDateList[cAbsoluteTime[First@date,CalendarType->type,TimeZone->OptionValue[TimeZone],Method->OptionValue[Method]]],
(*"ArithmeticPersian"(*|"AstronomicalPersian"*)|"Islamic",cCalendarConvert[cAbsoluteTime[First@date,CalendarType->type,TimeZone\[Rule]OptionValue[TimeZone],Method->OptionValue[Method]],"Gregorian",Method->OptionValue[Method]],*)
_,Message[cDateList::UnsupportedCalendar,type]
]



Options[cAbsoluteTime]={Method->Automatic,CalendarType->"Gregorian",TimeZone:>$TimeZone};
cAbsoluteTime::UnsupportedCalendar="Calendar \"`1`\" is not supported.";
SyntaxInformation[cAbsoluteTime]={"ArgumentsPattern"->{_.,OptionsPattern[]}};

(* special case for default *)
cAbsoluteTime[]:=cDateFunctions`LibraryLink`AbsoluteNowLink[]+$TimeZone*3600.
(*cAbsoluteTime[TimeZone->offset_]:=cDateFunctions`LibraryLink`AbsoluteNowLink[]+(offset-$TimeZone)*3600*)

cAbsoluteTime[date:{Repeated[_Integer,{1,5}]}]:=cDateFunctions`LibraryLink`DateListToSecondLink[date]
cAbsoluteTime[{date:Repeated[_Integer,{5}],second:_Integer|_Real}]:=cDateFunctions`LibraryLink`DateListToSecondLink[{date}]+second

cAbsoluteTime[date:_Integer|_Real,OptionsPattern[]]:=date+(OptionValue[TimeZone]-$TimeZone)*3600.
cAbsoluteTime[OptionsPattern[]]:=cDateFunctions`LibraryLink`AbsoluteNowLink[]+OptionValue[TimeZone]*3600.


(*support for version 13 format*)
cAbsoluteTime[DateObject[date_,_]]:=cAbsoluteTime[date]

(* special case for gregorian *)
cAbsoluteTime[DateObject[date_,_,"Gregorian",timeZone_:$TimeZone]]:=cAbsoluteTime[date]+($TimeZone-timeZone)*3600.




cAbsoluteTime[{date:Repeated[_Integer,{5}],second:_Integer|_Real},OptionsPattern[]]:=Switch[OptionValue[CalendarType],
"Gregorian",cDateFunctions`LibraryLink`DateListToSecondLink[{date}]+second+(OptionValue[TimeZone]-$TimeZone)*3600.,
"ArithmeticPersian",cDateFunctions`LibraryLink`ArithmeticPersianDateListToSecondLink[cDateFunctions`ArithmeticPersian`FindCycleID[OptionValue[Method]],{date}]+second-cDateFunctions`ArithmeticPersian`Constants`OriginOffsetFromAbsoluteTime+(OptionValue[TimeZone]-$TimeZone)*3600.,
(*"AstronomicalPersian",cDateFunctions`LibraryLink`AstronomicalPersianDateListToSecondLink[{date}]+second-cDateFunctions`ArithmeticPersian`Constants`OriginOffsetFromAbsoluteTime+(OptionValue[TimeZone]-$TimeZone)*3600.,*)
"Islamic",cDateFunctions`LibraryLink`IslamicDateListToSecondLink[cDateFunctions`Islamic`FindCycleID[OptionValue[Method]],{date}]+second-cDateFunctions`Islamic`Constants`OriginOffsetFromAbsoluteTime+(OptionValue[TimeZone]-$TimeZone)*3600.,
_,Message[cAbsoluteTime::UnsupportedCalendar,OptionValue[CalendarType]]
]

cAbsoluteTime[date:{Repeated[_Integer,{1,5}]},OptionsPattern[]]:=cAbsoluteTime[PadRight[date,6],CalendarType->OptionValue[CalendarType],TimeZone->OptionValue[TimeZone],Method->OptionValue[Method]]

cAbsoluteTime[DateObject[date_,_,type_:"Gregorian",timeZone_:$TimeZone],OptionsPattern[]]:=cAbsoluteTime[date,CalendarType->type,TimeZone->timeZone,Method->OptionValue[Method]]

cAbsoluteTime[time_TimeObject]:=Dot[First@time,{3600,60,1}]

cHolidayQ2[date:{Repeated[_Integer,{1,5}]}|{Repeated[_Integer,{5}],_Integer|_Real}]:=cDateFunctions`LibraryLink`HolidayQLink[{date}]

Options[cLeapYearQ]={CalendarType->Automatic,Method->Automatic};
SyntaxInformation[cLeapYearQ]={"ArgumentsPattern"->{_,OptionsPattern[]}};
cLeapYearQ::UnsuopportedCalendarType="Calendar type \"`1`\" is not supported.";

cLeapYearQ[AbsoluteValue:_Integer|_Real,OptionsPattern[]]:=cLeapYearQ[cDateList[AbsoluteValue],CalendarType->OptionValue[CalendarType],Method->OptionValue[Method]]

cLeapYearQ[date:{Repeated[_Integer,{1,5}]}|{Repeated[_Integer,{5}],_Integer|_Real},OptionsPattern[]]:=Switch[OptionValue[CalendarType],
Automatic|"Gregorian",cDateFunctions`LibraryLink`GregorianIsLeapYearLink[First@cDateFunctions`LibraryLink`SecondToDateListLink[cAbsoluteTime[date]]],
"ArithmeticPersian",Block[{methodID=cDateFunctions`ArithmeticPersian`FindCycleID[OptionValue[Method]]},cDateFunctions`LibraryLink`ArithmeticPersianIsLeapYearLink[methodID,First@cDateFunctions`LibraryLink`ArithmeticPersianSecondToDateListLink[methodID,cDateFunctions`ArithmeticPersian`DateListToSecond[methodID,date]]]],
(*"AstronomicalPersian",cDateFunctions`LibraryLink`AstronomicalPersianIsLeapYearLink[First@cDateFunctions`LibraryLink`AstronomicalPersianSecondToDateListLink[cDateFunctions`AstronomicalPersian`DateListToSecond[date]]],*)
"Islamic",Block[{methodID=cDateFunctions`Islamic`FindCycleID[OptionValue[Method]]},cDateFunctions`LibraryLink`IslamicIsLeapYearLink[methodID,First@cDateFunctions`LibraryLink`IslamicSecondToDateListLink[methodID,cDateFunctions`Islamic`DateListToSecond[methodID,date]]]],
_,Message[cLeapYearQ::UnsuopportedCalendarType,OptionValue[CalendarType]]
]

cLeapYearQ[DateObject[date_,_]]:=cDateFunctions`LibraryLink`GregorianIsLeapYearLink[First@date]

cLeapYearQ[date:DateObject[{year_,___},"Year"|"Month"|"Quarter"|"Week"|"WeekBeginningSunday"|"Day"|"Hour"|"Minute"|"Second"|"Instant",type_:"Gregorian",_:0],OptionsPattern[]]:=If[OptionValue[CalendarType]===Automatic,
Switch[type,
"Gregorian",cLeapYearQ[{year}],
"ArithmeticPersian",cLeapYearQ[{year},CalendarType->"ArithmeticPersian"],
"AstronomicalPersian",cLeapYearQ[{year},CalendarType->"AstronomicalPersian"],
"Islamic",cLeapYearQ[{year},CalendarType->"Islamic"],
_,Message[cLeapYearQ::UnsuopportedCalendarType,OptionValue[CalendarType]]
],

(*CalendarType is given*)If[type=!=OptionValue[CalendarType],
cLeapYearQ[cCalendarConvert[date,OptionValue[CalendarType]],OptionValue[CalendarType]],
cLeapYearQ[{year},CalendarType->type]
]
]

(*Gregorian section*)
cDateFunctions`Gregorian`DateListToSecond[{date:Repeated[_Integer,{5}],second:_Integer|_Real}]:=cDateFunctions`LibraryLink`DateListToSecondLink[{date}]+second
cDateFunctions`Gregorian`DateListToSecond[date:{Repeated[_Integer,{1,5}]}]:=cDateFunctions`LibraryLink`DateListToSecondLink[date]
cDateFunctions`Gregorian`SecondToDateList[date:_Integer|_Real]:=Append[cDateFunctions`LibraryLink`SecondToDateListLink[date],Mod[date,60]]

(*ArithmeticPersian Section*)
cDateFunctions`ArithmeticPersian`Constants`OriginOffsetFromAbsoluteTime=40322880000.;

cDateFunctions`ArithmeticPersian`SecondToDateList[cycleID_Integer:2,date:_Integer|_Real]:=Append[cDateFunctions`LibraryLink`ArithmeticPersianSecondToDateListLink[cycleID,Floor@date],Mod[date,60.]]

cDateFunctions`ArithmeticPersian`DateListToSecond[cycleID_Integer:2,{date:Repeated[_Integer,{5}],second:_Integer|_Real}]:=cDateFunctions`LibraryLink`ArithmeticPersianDateListToSecondLink[cycleID,{date}]+second

cDateFunctions`ArithmeticPersian`DateListToSecond[cycleID_Integer:2,date:{Repeated[_Integer,{1,5}]}]:=cDateFunctions`LibraryLink`ArithmeticPersianDateListToSecondLink[cycleID,date]

cDateFunctions`ArithmeticPersian`ConvertToAbsoluteTimeToDateList[cycleID_Integer:2,date_]:=Append[cDateFunctions`LibraryLink`ArithmeticPersianSecondToDateListLink[cycleID,Floor@#],Mod[#,60.]]&[cDateFunctions`ArithmeticPersian`Constants`OriginOffsetFromAbsoluteTime+cAbsoluteTime[date]]

(*todo arithmetic`datelisToDateList*)

cDateFunctions`ArithmeticPersian`FindCycleID::InvalidMethod="Default method will be used because \"`1`\" is not a valid method.";
cDateFunctions`ArithmeticPersian`FindCycleID[type_Integer]:=type;
cDateFunctions`ArithmeticPersian`FindCycleID[type_]:=Switch[type,"Cycle-33",0,"Cycle-128",1,"Cycle-2820-1",2,"Cycle-2820-2",3,"Cycle-4166",4,_,If[type=!=Automatic,Message[cDateFunctions`ArithmeticPersian`FindCycleID::InvalidMethod,type]];2]

(*AstronomicalPersian Section*)

(*cDateFunctions`AstronomicalPersian`SecondToDateList[date:_Integer|_Real]:=Append[cDateFunctions`LibraryLink`AstronomicalPersianSecondToDateListLink[Floor@date],Mod[date,60.]]

cDateFunctions`AstronomicalPersian`DateListToSecond[{date:Repeated[_Integer,{5}],second:_Integer|_Real}]:=cDateFunctions`LibraryLink`AstronomicalPersianDateListToSecondLink[{date}]+second

cDateFunctions`AstronomicalPersian`DateListToSecond[date:{Repeated[_Integer,{1,5}]}]:=cDateFunctions`LibraryLink`AstronomicalPersianDateListToSecondLink[date]

cDateFunctions`AstronomicalPersian`ConvertToAbsoluteTimeToDateList[date_]:=Append[cDateFunctions`LibraryLink`AstronomicalPersianSecondToDateListLink[Floor@#],Mod[#,60.]]&[cDateFunctions`ArithmeticPersian`Constants`OriginOffsetFromAbsoluteTime+cAbsoluteTime[date]]*)


(*Islamic Section*)

cDateFunctions`Islamic`Constants`OriginOffsetFromAbsoluteTime=40312598400;

cDateFunctions`Islamic`SecondToDateList[cycleID_Integer:1,date:_Integer|_Real]:=Append[cDateFunctions`LibraryLink`IslamicSecondToDateListLink[cycleID,Floor@date],Mod[date,60.]]

cDateFunctions`Islamic`DateListToSecond[cycleID_Integer:1,{date:Repeated[_Integer,{5}],second:_Integer|_Real}]:=cDateFunctions`LibraryLink`IslamicDateListToSecondLink[cycleID,{date}]+second

cDateFunctions`Islamic`DateListToSecond[cycleID_Integer:1,date:{Repeated[_Integer,{1,5}]}]:=cDateFunctions`LibraryLink`IslamicDateListToSecondLink[cycleID,date]

cDateFunctions`Islamic`ConvertToAbsoluteTimeToDateList[cycleID_Integer:1,date_]:=Append[cDateFunctions`LibraryLink`IslamicSecondToDateListLink[cycleID,Floor@#],Mod[#,60.]]&[cDateFunctions`Islamic`Constants`OriginOffsetFromAbsoluteTime+cAbsoluteTime[date]]

cDateFunctions`Islamic`FindCycleID::InvalidMethod="Default method will be used because \"`1`\" is not a valid method.";
cDateFunctions`Islamic`FindCycleID[type_Integer]:=type;
cDateFunctions`Islamic`FindCycleID[type_]:=Switch[type,"15-Based",0,"16-Based",1,"Indian",2,"Habash al-Hasib",3,_,If[type=!=Automatic,Message[cDateFunctions`Islamic`FindCycleID::InvalidMethod,type]];1]

(* used internally, Only date format list will be passed - no checking needed. *)
cDateFunctions`Private`GetCalendarType[DateObject[_,_]|_List]="Gregorian";
cDateFunctions`Private`GetCalendarType[DateObject[_,_,type_,_]|DateObject[_,_,type_:"Gregorian"]]:=type;

cCalendarConvert::unsupportedInput="The argument \"`1`\" is not supported.";
cCalendarConvert::unsupportedFormat="The format \"`1`\" is not supported.";

Options[cCalendarConvert]={Method->Automatic};
SyntaxInformation[cCalendarConvert]={"ArgumentsPattern"->{_,_,OptionsPattern[]}};
(*Number:Gregorian absolute value*)
cCalendarConvert[date:_Integer|_Real|_DateObject|{Repeated[_Integer,{1,5}]}|{Repeated[_Integer,{5}],_Integer|_Real},format_String,OptionsPattern[]]:=Switch[format,
"ArithmeticPersian",Switch[date,
_Integer|_Real,cDateFunctions`ArithmeticPersian`SecondToDateList[cDateFunctions`ArithmeticPersian`FindCycleID[OptionValue[Method]],Floor@date+cDateFunctions`ArithmeticPersian`Constants`OriginOffsetFromAbsoluteTime],
_List|_DateObject,Switch[cDateFunctions`Private`GetCalendarType[date],
"Gregorian",cDateFunctions`ArithmeticPersian`ConvertToAbsoluteTimeToDateList[cDateFunctions`ArithmeticPersian`FindCycleID[OptionValue[Method]],date],
"Islamic",cDateFunctions`ArithmeticPersian`SecondToDateList[cDateFunctions`ArithmeticPersian`FindCycleID[OptionValue[Method]],cDateFunctions`Islamic`DateListToSecond[First@date]+10281600],
"ArithmeticPersian",cDateFunctions`Private`ToDateListFormat[First@date],
(*"AstronomicalPersian",cDateFunctions`ArithmeticPersian`SecondToDateList[cDateFunctions`ArithmeticPersian`FindCycleID[OptionValue[Method]],cDateFunctions`AstronomicalPersian`DateListToSecond[First@date]],*)
_,Message[cCalendarConvert::unsupportedFormat,cDateFunctions`Private`GetCalendarType[date]]],
_,Message[cCalendarConvert::unsupportedInput,date]
],
(*"AstronomicalPersian",Switch[date,
_Integer|_Real,cDateFunctions`AstronomicalPersian`SecondToDateList[Floor@date+cDateFunctions`ArithmeticPersian`Constants`OriginOffsetFromAbsoluteTime],
_List|_DateObject,Switch[cDateFunctions`Private`GetCalendarType[date],
"Gregorian",cDateFunctions`AstronomicalPersian`ConvertToAbsoluteTimeToDateList[date],
"Islamic",cDateFunctions`AstronomicalPersian`SecondToDateList[cDateFunctions`Islamic`DateListToSecond[cDateFunctions`Islamic`FindCycleID[OptionValue[Method]],First@date]+10281600],
"ArithmeticPersian",cDateFunctions`AstronomicalPersian`SecondToDateList[cDateFunctions`ArithmeticPersian`DateListToSecond[cDateFunctions`ArithmeticPersian`FindCycleID[OptionValue[Method]],First@date]],
"AstronomicalPersian",cDateFunctions`Private`ToDateListFormat[First@date],
_,Message[cCalendarConvert::unsupportedFormat,cDateFunctions`Private`GetCalendarType[date]]],
_,Message[cCalendarConvert::unsupportedInput,date]
],*)
"Gregorian",Switch[date,
_Integer|_Real|_List,cDateList[date],
_DateObject,Switch[cDateFunctions`Private`GetCalendarType[date],
"Gregorian",cDateFunctions`Private`ToDateListFormat[First@date],
"ArithmeticPersian",cDateList[cDateFunctions`ArithmeticPersian`DateListToSecond[cDateFunctions`ArithmeticPersian`FindCycleID[OptionValue[Method]],First@date]-cDateFunctions`ArithmeticPersian`Constants`OriginOffsetFromAbsoluteTime],
(*"AstronomicalPersian",cDateList[cDateFunctions`AstronomicalPersian`DateListToSecond[First@date]-cDateFunctions`ArithmeticPersian`Constants`OriginOffsetFromAbsoluteTime],*)
"Islamic",cDateList[cDateFunctions`Islamic`DateListToSecond[cDateFunctions`Islamic`FindCycleID[OptionValue[Method]],First@date]-cDateFunctions`Islamic`Constants`OriginOffsetFromAbsoluteTime],
_,Message[cCalendarConvert::unsupportedFormat,cDateFunctions`Private`GetCalendarType[date]]],
_,Message[cCalendarConvert::unsupportedInput,date]
],
"Islamic",Switch[date,
_Integer|_Real,cDateFunctions`Islamic`SecondToDateList[cDateFunctions`Islamic`FindCycleID[OptionValue[Method]],Floor@date+cDateFunctions`Islamic`Constants`OriginOffsetFromAbsoluteTime],
_List|_DateObject,Switch[cDateFunctions`Private`GetCalendarType[date],
"Gregorian",cDateFunctions`Islamic`ConvertToAbsoluteTimeToDateList[cDateFunctions`Islamic`FindCycleID[OptionValue[Method]],date],
"Islamic",cDateFunctions`Private`ToDateListFormat[First@date],
"ArithmeticPersian",cDateFunctions`Islamic`SecondToDateList[cDateFunctions`Islamic`FindCycleID[OptionValue[Method]],cDateFunctions`ArithmeticPersian`DateListToSecond[First@date]-10281600],
(*"AstronomicalPersian",cDateFunctions`Islamic`SecondToDateList[cDateFunctions`Islamic`FindCycleID[OptionValue[Method]],cDateFunctions`AstronomicalPersian`DateListToSecond[First@date]-10281600],*)
_,Message[cCalendarConvert::unsupportedFormat,cDateFunctions`Private`GetCalendarType[date]]],
_,Message[cCalendarConvert::unsupportedInput,date]
],
_,Message[cCalendarConvert::unsupportedFormat,format]
]

ClearAll[cDateFunctions`Gregorian`MonthView,cDateFunctions`Gregorian`YearView,cDateFunctions`ArithmeticPersian`MonthView,cDateFunctions`ArithmeticPersian`YearView,cDateFunctions`Islamic`MonthView,cDateFunctions`Islamic`YearView,cDateFunctions`Private`ViewGenerator,cDateFunctions`Private`MonthViewGenerator,cDateFunctions`Private`YearViewGenerator,cMonthView,cMonthView];

cDateFunctions`Gregorian`MonthView[date:_Integer|_Real|{Repeated[_Integer,{1,5}]}|{Repeated[_Integer,{5}],_Real}]:=cDateFunctions`LibraryLink`GregorianMonthViewLink[Floor@cAbsoluteTime[date]]
cDateFunctions`Gregorian`YearView[start:_Integer|_Real|{Repeated[_Integer,{1,5}]}|{Repeated[_Integer,{5}],_Real},end:_Integer|_Real|{Repeated[_Integer,{1,5}]}|{Repeated[_Integer,{5}],_Real}]:=cDateFunctions`LibraryLink`GregorianYearViewLink[Floor@cAbsoluteTime[start],Floor@cAbsoluteTime[end]]

cDateFunctions`ArithmeticPersian`MonthView[cycleID_:2,date:_Integer|_Real|{Repeated[_Integer,{1,5}]}|{Repeated[_Integer,{5}],_Real}]:=cDateFunctions`LibraryLink`ArithmeticPersianMonthViewLink[cycleID,Floor@cDateFunctions`ArithmeticPersian`DateListToSecond[cycleID,date]]
cDateFunctions`ArithmeticPersian`YearView[cycleID_:2,start:_Integer|_Real|{Repeated[_Integer,{1,5}]}|{Repeated[_Integer,{5}],_Real},end:_Integer|_Real|{Repeated[_Integer,{1,5}]}|{Repeated[_Integer,{5}],_Real}]:=cDateFunctions`LibraryLink`ArithmeticPersianYearViewLink[cycleID,Floor@cDateFunctions`ArithmeticPersian`DateListToSecond[cycleID,start],Floor@cDateFunctions`ArithmeticPersian`DateListToSecond[cycleID,end]]

(*cDateFunctions`AstronomicalPersian`MonthView[date:_Integer|_Real|{Repeated[_Integer,{1,5}]}|{Repeated[_Integer,{5}],_Real}]:=cDateFunctions`LibraryLink`AstronomicalPersianMonthViewLink[Floor@cDateFunctions`AstronomicalPersian`DateListToSecond[date]]
cDateFunctions`AstronomicalPersian`YearView[start:_Integer|_Real|{Repeated[_Integer,{1,5}]}|{Repeated[_Integer,{5}],_Real},end:_Integer|_Real|{Repeated[_Integer,{1,5}]}|{Repeated[_Integer,{5}],_Real}]:=cDateFunctions`LibraryLink`AstronomicalPersianYearViewLink[Floor@cDateFunctions`AstronomicalPersian`DateListToSecond[start],Floor@cDateFunctions`AstronomicalPersian`DateListToSecond[end]]*)

cDateFunctions`Islamic`MonthView[cycleID_:1,date:_Integer|_Real|{Repeated[_Integer,{1,5}]}|{Repeated[_Integer,{5}],_Real}]:=cDateFunctions`LibraryLink`IslamicMonthViewLink[cycleID,Floor@cDateFunctions`Islamic`DateListToSecond[cycleID,date]]
cDateFunctions`Islamic`YearView[cycleID_:1,start:_Integer|_Real|{Repeated[_Integer,{1,5}]}|{Repeated[_Integer,{5}],_Real},end:_Integer|_Real|{Repeated[_Integer,{1,5}]}|{Repeated[_Integer,{5}],_Real}]:=cDateFunctions`LibraryLink`IslamicYearViewLink[cycleID,Floor@cDateFunctions`Islamic`DateListToSecond[cycleID,start],Floor@cDateFunctions`Islamic`DateListToSecond[cycleID,end]]

cDateFunctions`Private`ViewGenerator[data_List,title_:"",labelingOrder_Integer:0]:=Labeled[TableForm[Replace[Partition[ArrayPad[data,{labelingOrder,0},""],7,7,1,""],{"","","","","","",""}->Nothing,{1}],
	TableHeadings -> {None,RotateRight[{"M","Tu", "W", "Th", "F", "Sa","Su"},labelingOrder]}, 
   TableSpacing -> {2, 2}, TableAlignments -> {Center, Center}]
   , ToString[title], Top,LabelStyle->11]

cDateFunctions`Private`MonthViewGenerator[{offset_,length_},title_:"",labelingOrder_Integer:0]:=cDateFunctions`Private`ViewGenerator[PadLeft[Range[length],offset+length,""],title,labelingOrder]

cDateFunctions`Private`YearViewGenerator[{offset_,start_,lengths:Repeated[_Integer,{1,Infinity}]},title_:"",labelingOrder_Integer:0]:=cDateFunctions`Private`ViewGenerator[Block[{lens={lengths}},Join[PadLeft[Range[start,First@lens],offset+(First@lens-start+1),""],Catenate[Range/@Rest[lens]]]],title,labelingOrder]

Options[cMonthView]={CalendarType->Automatic,Method->Automatic,Order->Automatic};
SyntaxInformation[cMonthView]={"ArgumentsPattern"->{_,_.,OptionsPattern[]}};
cMonthView::UnsupportedCalendar="Calendar type \"`1`\" is not supported.";
cMonthView[date:_Integer|_Real|{Repeated[_Integer,{1,5}]}|{Repeated[_Integer,{5}],_Real},Optional[title:_String|_Symbol,Automatic],OptionsPattern[]]:=Switch[OptionValue[CalendarType],
Automatic|"Gregorian",cDateFunctions`Private`MonthViewGenerator[cDateFunctions`Gregorian`MonthView[date],If[title===Automatic,StringRiffle[PadRight[date,2,1],"-"],title],If[OptionValue[Order]===Automatic,1,OptionValue[Order]]],
"ArithmeticPersian",cDateFunctions`Private`MonthViewGenerator[cDateFunctions`ArithmeticPersian`MonthView[cDateFunctions`ArithmeticPersian`FindCycleID[OptionValue[Method]],date],If[title===Automatic,"[ArithmeticPersian] "<>StringRiffle[PadRight[date,2,1],"-"],title],If[OptionValue[Order]===Automatic,2,OptionValue[Order]]],
(*"AstronomicalPersian",cDateFunctions`Private`MonthViewGenerator[cDateFunctions`AstronomicalPersian`MonthView[date],If[title===Automatic,"[AstronomicalPersian] "<>StringRiffle[PadRight[date,2,1],"-"],title],If[OptionValue[Order]===Automatic,2,OptionValue[Order]]],*)
"Islamic",cDateFunctions`Private`MonthViewGenerator[cDateFunctions`Islamic`MonthView[cDateFunctions`Islamic`FindCycleID[OptionValue[Method]],date],If[title===Automatic,"[Islamic] "<>StringRiffle[PadRight[date,2,1],"-"],title],If[OptionValue[Order]===Automatic,2,OptionValue[Order]]],
_,Message[cMonthView::UnsupportedCalendar,OptionValue[CalendarType]]
]


Options[cPartialView]={CalendarType->Automatic,Method->Automatic,Order->Automatic};
SyntaxInformation[cPartialView]={"ArgumentsPattern"->{_,_,_.,OptionsPattern[]}};
cPartialView::UnsupportedCalendar="Calendar type \"`1`\" is not supported.";
(*cPartialView[start:{Repeated[_Integer,{1,5}]}|{Repeated[_Integer,{5}],_Real},end:{Repeated[_Integer,{1,5}]}|{Repeated[_Integer,{5}],_Real},Optional[title:_String|_Symbol,Automatic](*Optional[Pattern[title,Except[Rule]],Automatic]*),OptionsPattern[]]/;cAbsoluteTime[start]>cAbsoluteTime[end]:=cPartialView[end,start,title,Sequence@@Normal@AssociationMap[OptionValue[cPartialView,#]&,Keys@Options[cPartialView]]]*)
cPartialView[startDate:{Repeated[_Integer,{1,5}]}|{Repeated[_Integer,{5}],_Real},endDate:{Repeated[_Integer,{1,5}]}|{Repeated[_Integer,{5}],_Real},Optional[title:_String|_Symbol,Automatic],OptionsPattern[]]:=Block[{start,end},
If[cAbsoluteTime[startDate,CalendarType->OptionValue[CalendarType]]>cAbsoluteTime[endDate,CalendarType->OptionValue[CalendarType]],start=endDate;end=startDate;,start=startDate;end=endDate;];
Switch[OptionValue[CalendarType],
Automatic|"Gregorian",cDateFunctions`Private`YearViewGenerator[cDateFunctions`Gregorian`YearView[start,end],If[title===Automatic,StringRiffle[PadRight[start,3,1],"-"]<>" -> "<>StringRiffle[PadRight[cDateList@end,3,1],"-"],title],If[OptionValue[Order]===Automatic,1,OptionValue[Order]]],
"ArithmeticPersian",cDateFunctions`Private`YearViewGenerator[cDateFunctions`ArithmeticPersian`YearView[cDateFunctions`ArithmeticPersian`FindCycleID[OptionValue[Method]],start,end],If[title===Automatic,"[ArithmeticPersian] "<>StringRiffle[PadRight[start,2,1],"-"]<>" -> "<>StringRiffle[PadRight[end,2,1],"-"],title],If[OptionValue[Order]===Automatic,2,OptionValue[Order]]],
(*"AstronomicalPersian",cDateFunctions`Private`YearViewGenerator[cDateFunctions`AstronomicalPersian`YearView[start,end],If[title===Automatic,"[AstronomicalPersian] "<>StringRiffle[PadRight[start,2,1],"-"]<>" -> "<>StringRiffle[PadRight[end,2,1],"-"],title],If[OptionValue[Order]===Automatic,2,OptionValue[Order]]],*)
"Islamic",cDateFunctions`Private`YearViewGenerator[cDateFunctions`Islamic`YearView[cDateFunctions`Islamic`FindCycleID[OptionValue[Method]],start,end],If[title===Automatic,"[Islamic] "<>StringRiffle[PadRight[start,2,1],"-"]<>" -> "<>StringRiffle[PadRight[end,2,1],"-"],title],If[OptionValue[Order]===Automatic,2,OptionValue[Order]]],
_,Message[cMonthView::UnsupportedCalendar,OptionValue[CalendarType]]
]
]



End[];
EndPackage[];
