(* ::Package:: *)

(* Mathematica Package *)

(* Title:     cDateFunctions *)
(* Author:    Benjamin Izadpanah *)
(* Copyright: Benjamin Izadpanah *)
(* Contact:   ben.izd@outlook.com *)

(* Summary:   This package includes functions equivalent to Wolfram Language built-in date-related functions with minimum supported options. 
			  TimeZone,TimeSystem are not directly supported. *)
			  
(* Context:   cDateFunctions` *)			  
(* Context Conflict: If you used the cDateFunctions LibraryLink package (uses DLL files), cLeapYearQ will be overridden to use pure Wolfram-Language implementation. *)			  
(* Start Date:         2021-7 *)
(* Last Modified Date: 2021-12 *)

(* version required, because:

12.3 , "FromDateString" used in cDateBounds,cDateWithinQ,cDateOverlapsQ to support string as a start argument

12.1 , "Splice" used in cHoliday*
       "DateInterval" used in cDatePlus to support it

11.0 , "Ramp" used in cCalendarRangeView to create calendrical view

10.4 , "MixedMagnitude","MixedUnit" used in cDateDifference for handling multiple types
       "Highlighted" used in cCalendarRangeView,cCalendarView,cCalendarMultipleView to highlight given dates

10.3 , "LessEqualThan","GreaterEqualThan" used in cDateRange

10.2 , "Nothing" was used to generate holidays, end/beginning of months

10.0 , "Catenate" used in cDateRange,cDayRange,cDayCount,cHoliday*
       "TimeObject" was used to be supported in different functions
       "GroupBy" used in cDatePlus to handle multiple units
       "AnyTrue" used in "DateWithinQ","DateOverlapsQ"
       "AllTrue" used in "DateWithinQ"
       "ColorQ" used to validate input in genrating hilighted calendrical views
       "FirstPosition" used in "cCalendarRangeView"
       "DateObject" used in many places, mainly to construct the ouput
*)

(* 81 main built-in functions used to build this package (not all the functions listed) *)

(* {Abs, AbsoluteTime, AllTrue, Alternatives, And, AnyTrue, 
Append, Apply, ArrayPad, ArrayQ, Begin, BeginPackage, 
BitXor, Block, Boole, Cases, Catch, Catenate, Ceiling, 
Check, Circle, ClearAll, Clip, ColorQ, Complement, 
ConstantArray, Count, DateInterval, DateList, DateObject, 
DateRange, DateString, Defer, DeleteCases, 
DeleteDuplicates, Dot, Drop, End, EndPackage, Except, 
First, FirstPosition, Floor, Fold, FoldList, 
FractionalPart, FreeQ, FromAbsoluteTime, FromDateString, 
Graphics, GrayLevel, GreaterEqualThan, GroupBy, Head, 
Highlighted, HoldComplete, Identity, If, Inset, IntegerQ, 
Intersection, Join, Labeled, Last, Length, LessEqualThan, 
Map, MapAt, MatchQ, Max, Message, Min, Missing, 
MissingQ, MixedMagnitude, MixedUnit, Mod, Most, N, 
Negative, Nest, NestWhileList, Normal, Not, Nothing, 
NumberQ, Optional, Or, Partition, Pink, Positive, 
Quantity, Quiet, Quotient, QuotientRemainder, Ramp, 
Range, Repeated, Replace, ReplaceAll, ReplacePart, Rest, 
Return, Reverse, Select, Sequence, SetAttributes, Sign, 
Sort, SortBy, Span, Splice, Style, Switch, 
SyntaxInformation, Table, TableForm, Take, Text, 
Thickness, Thread, Throw, TimeObject, Total, Transpose, 
Unevaluated, Unitize, UnitStep, With} *)

(* holidays:
1,1 - New Year day (year>=1871) - 101
1,third Monday - Martin Luther King (year>=1986) - 102
2,third Monday - Presidents' Day (22 feb for 1885 <= year < 1971, third monday for year >= 1971) - 201
5,last Monday - Memorial Day (year>=1971) - 501
6,19-juneteenth (year>=2021) - 601
7,4 - Independence Day (year>=1941) - 701
9,first monday - Labor Day (year>=1894) - 901
10,second Monday - Columbus Day (year>=1971) - 1001
10,12 - Columnus Day (1936<year<1971) - 1001
10,fourth monday - Veterans Day (1977>=year>=1971) - 1101
11,11 - Veterans Day (year>=1938 not in 1977>=year>=1971) - 1101
11,fourth Thursday - Thanksgiving (year>=1863) - 1102
12,25 - Christmas Day (year>=1871) - 1201

Source: www.timeanddate.com and www.britannica.com *)


BeginPackage["cDateFunctions`"];



(* ::Subsection:: *)
(*cLeapYearValue*)


cLeapYearQ::usage="Alternative to Mathematica built-in LeapYearQ.";
cLeapYearQ::invalidArgument="The argument `1` is not a valid day/list of days.";
SyntaxInformation[cLeapYearQ]={"ArgumentsPattern"->{_}};

ClearAll[cBusinessDayQ,cCurrentDate,cDateBounds,cDateDifference,cDateRange,
	cDateWithinQ, cDayCount, cDateOverlapsQ,cDatePlus,cDayMatchQ, cDayName,
cDayRound,cDayPlus, cDayRange,cPreviousDate, cNextDate,cHolidayName,cLeapYearQ,cCalendarView,cCalendarRangeView,cCalendarMultipleView];

Begin["`Private`"];

ClearAll[cAddDayOff, cAddDayWithPattern, cAddListDate, cAddListDateList, cAddMonth, cAddMonthList, cAddYear, cAddYearList,
cBeginningOfMonthRange, cCalculateDateLength,
cCalculateDays, cCalculateNumberOfDays, cConvertUnit,  cCustomDateList,  cDateBoundsCore, 
 cDateDifferenceCore, cDateDifferenceIntegerCore, cDateOverlapComparison,  cDatePlusNest, cDatePlusNestCore,
cDatePlusValue, cDatePrecision,  cDateWhitinComparison, cDayCountCore, 
 cDayNameIndex, cDayNameValue,  cDayRangeValue,cCircleAround,allowedHighlightsTypes,
cDayRoundBackwardValue, cDayRoundCore, cDayRoundValue, cEndOfMonthRange, cHolidayDaysFilter, cHolidayDaysGenerate,
cHolidayDaysToAbsolute, cHolidayNameCore, cHolidayQCore, cHolidayRange, cHolidayRangeCore, cHolidayYear, 
cLeapYearValue, cMergeUnits, cMonthList, cNextDateCore,  cPreviousDateCore, cQuantityToPair,
cUnitToQuantityUnit, cUnitValue,cConvertResultFrom,cNextHoliday,cPreviousHoliday,cCalendarCore,
cCalendarViewCore,cMapAt,cMapAtHighlight,cNumberDecomposeForTimeObject,cMonthListWithDay,yearDifference,monthDifference,quarterDifference,cHolidayQ];

(* cLeapYearValue[{2019, 2020, 2021}] -> {0, 1, 0} *)
cLeapYearValue[year:{__Integer}] := Block[{refinedYears=year + Boole@Negative@year},BitXor[1,Unitize[Mod[refinedYears,4]],Unitize[Mod[refinedYears,100]],Unitize[Mod[refinedYears,400]]]]

(* single start - cLeapYearQ[{2020,1,1}] *)
cLeapYearQ[year:_Integer|_Real|_DateObject|_String|{Repeated[_Integer|_Real,{2, 6}]}]:=cLeapYearValue[{First[DateList[year]]}]==={1}

(* single year - cLeapYearQ[{2020}] *)
cLeapYearQ[year:{_Integer}] := cLeapYearValue[year] === {1}

(* repeated start cLeapYearQ[{{2020,1,1},{2021,1,1}}] *)
cLeapYearQ[years:{Repeated[_Integer|_Real,{7, Infinity}]}|{({Repeated[_Integer|_Real,{1,6}]}|_String)..}]:=Thread[cLeapYearValue[(DateList /@ years)[[All,1]]]==1]

(* pick year directly from DateObject *)
cLeapYearQ[years:{__DateObject}]:=Thread[cLeapYearValue[years[[All,1,1]]]==1]

(* convert all elements to integer *)
cLeapYearQ[years:{{_Integer|_Real}..}]:=Thread[cLeapYearValue[Apply[Floor,years,{1}]]==1]

End[];



(* ::Subsection:: *)
(*monthDifference, quarterDifference, yearDifference*)


Begin["`Private`"];

(* return AbsoluteTime -  add the second argument to the first argument which is a DateList + checking to be on the exact day as the input *)
cAddListDate[date:{Repeated[_Integer|_Real,{6}]},list:{Repeated[_Integer|_Real,{6}]}]:=Block[{temp=DateList[date+list]},
If[date[[3]]==temp[[3]],AbsoluteTime[temp],AbsoluteTime[temp]-temp[[3]]*86400.]
]

(* return DateList - same as cAddListDate *)
cAddListDateList[date:{Repeated[_Integer|_Real,{6}]},list:{Repeated[_Integer|_Real,{6}]}]:=Block[{temp=DateList[date+list]},
If[date[[3]]==temp[[3]],temp,DateList[temp-{0,0,temp[[3]],0,0,0}]]
]

(* specialized cases of cAddListDate/cAddListDateList *)
(* date should have in-bound DateList values *)
cAddMonth[date:{Repeated[_Integer|_Real,{6}]},month_Integer]:=cAddListDate[date,{0,month(* +Echo@Switch[{First@date,First@date+Quotient[month,12,1]},{_?Negative, _?(Not@*Negative)},12,{_?Positive, _?Negative},-12,_,0] *),0,0,0,0}]
(* cAddMonthList[date:{Repeated[_Integer|_Real,{6}]},month_Integer]:=Block[{result=cAddListDateList[date,{0,month,0,0,0,0}]},If[First@result==-1&&Positive[month],result+{1,0,0,0,0,0},result]] *)
cAddMonthList[date:{Repeated[_Integer|_Real,{6}]},month_Integer]:=cAddListDateList[date,{0,month,0,0,0,0}]

cAddYear[date:{Repeated[_Integer|_Real,{6}]},year_Integer]:=cAddListDate[date,{year+Switch[{First@date, First@date+year},
        {_?Negative, _?(Not@*Negative)}, 1,
        {_?Positive, _?Negative}, - 1,
        _, 0],0,0,0,0,0}]
(* cAddYear[date:{Repeated[_Integer|_Real,{6}]},year_Integer]:=cAddListDate[date,{If[First@date+year==0&&Positive[year],year+1,year],0,0,0,0,0}] *)
cAddYearList[date:{Repeated[_Integer|_Real,{6}]},year_Integer]:=cAddListDateList[date,{
    year+Switch[{First@date, First@date+year},
        {_?Negative, _?(Not@*Negative)}, 1,
        {_?Positive, _?Negative}, - 1,
        _, 0]
 ,0,0,0,0,0}]

(* Examples *)
(* cAddMonthList[{2020,1,31,0,0,0},1] -> {2020,2,29,0,0,0} *)
(* cAddYearList[{2020,2,29,0,0,0},1] -> {2021,2,28,0,0,0} *)

(* in monthDifference/quarterDifference/yearDifference, start start should be less than end start otherwise it'll return the negative output of reversed inputs *)
(* used inside cDateDifferenceCore *)

SetAttributes[monthDifference,Listable];
(* monthDifference[start:_Integer|_Real,end:_Integer|_Real]/;start>end:=-1*monthDifference[end,start] *)
monthDifference[start:_Integer|_Real,end:_Integer|_Real]/;start>end:=Block[{startDate=DateList[start],endDate=DateList[end],month,nextMonth,lastMonth,lastMonthList},
month=(First[endDate]-First[startDate])*12+(endDate[[2]]-startDate[[2]]);

(* Without Zero *)
If[Sign[First@startDate]!=Sign[First@endDate],month+=12];

If[cAddMonth[startDate,month]<end,month+=1];

lastMonthList=cAddMonthList[startDate,month]; 
lastMonth=AbsoluteTime[lastMonthList];
nextMonth=cAddMonth[lastMonthList,-1];

Return[month-(N@end-lastMonth)/(nextMonth-lastMonth)];
]

monthDifference[start:_Integer|_Real,end:_Integer|_Real]:=Block[{startDate=DateList[start],endDate=DateList[end],month,nextMonth,lastMonth,lastMonthList},
month=(First[endDate]-First[startDate])*12+(endDate[[2]]-startDate[[2]]);

(* Without Zero *)
If[Sign[First@startDate]!=Sign[First@endDate],month-=12];

If[cAddMonth[startDate,month]>end,month-=1];

lastMonthList=cAddMonthList[startDate,month]; 
lastMonth=AbsoluteTime[lastMonthList];
nextMonth=cAddMonth[lastMonthList,1];

Return[month+(N@end-lastMonth)/(nextMonth-lastMonth)];
]

SetAttributes[quarterDifference,Listable];
(* quarterDifference[start:_Integer|_Real,end:_Integer|_Real]/;start>end:=-1*quarterDifference[end,start] *)

(* quarterDifference[start:_Integer|_Real,end:_Integer|_Real]/;start>end:=-1*quarterDifference[end,start] *)
quarterDifference[start:_Integer|_Real,end:_Integer|_Real]/;start>end:=Block[{startDate=DateList[start],endDate=DateList[end],quarter,nextQuarter,lastQuarter,lastQuarterList},
quarter=Quotient[(First[endDate]-First[startDate])*12.+(endDate[[2]]-startDate[[2]]),3];

(* Without Zero *)
If[Sign[First@startDate]!=Sign[First@endDate],quarter+=4];

If[cAddMonth[startDate,quarter*3]<end,quarter+=1];

lastQuarterList=cAddMonthList[startDate,quarter*3]; 
lastQuarter=AbsoluteTime[lastQuarterList];
nextQuarter=cAddMonth[lastQuarterList,-3];

Return[quarter-(N[end]-lastQuarter)/(nextQuarter-lastQuarter)];
]

quarterDifference[start:_Integer|_Real,end:_Integer|_Real]:=Block[{startDate=DateList[start],endDate=DateList[end],quarter,nextQuarter,lastQuarter,lastQuarterList},
quarter=Quotient[(First[endDate]-First[startDate])*12.+(endDate[[2]]-startDate[[2]]),3];

(* Without Zero *)
If[Sign[First@startDate]!=Sign[First@endDate],quarter-=4];

If[cAddMonth[startDate,quarter*3]>end,quarter-=1];

lastQuarterList=cAddMonthList[startDate,quarter*3]; 
lastQuarter=AbsoluteTime[lastQuarterList];
nextQuarter=cAddMonth[lastQuarterList,3];

Return[quarter+(N[end]-lastQuarter)/(nextQuarter-lastQuarter)];
]

SetAttributes[yearDifference,Listable];
(* yearDifference[start:_Integer|_Real,end:_Integer|_Real]/;start>end:=-1*yearDifference[end,start] *)
yearDifference[start:_Integer|_Real,end:_Integer|_Real]/;start>end:=Block[{startDate=DateList[start],endDate=DateList[end],year,nextYear,lastYear,lastYearList},
year=First[endDate]-First[startDate];

(* Without Zero *)
If[Sign[First@startDate]!=Sign[First@endDate],year+=1];

If[cAddYear[startDate,year]<end,year+=1];
lastYearList=cAddYearList[startDate,year];
lastYear=AbsoluteTime[lastYearList];
nextYear=cAddYear[lastYearList,-1];

Return[year-(N[end]-lastYear)/(nextYear-lastYear)];
]

yearDifference[start:_Integer|_Real,end:_Integer|_Real]:=Block[{startDate=DateList[start],endDate=DateList[end],year,nextYear,lastYear,lastYearList},
year=First[endDate]-First[startDate];

(* Without Zero *)
If[Sign[First@startDate]!=Sign[First@endDate],year-=1];

If[cAddYear[startDate,year]>end,year-=1];

lastYearList=cAddYearList[startDate,year];
lastYear=AbsoluteTime[lastYearList];
nextYear=cAddYear[lastYearList,1];

Return[year+(N[end]-lastYear)/(nextYear-lastYear)];
]

End[];



(* ::Subsection:: *)
(*cDateDifference*)


cDateDifference::usage="Alternative to Mathematica built-in DateDifference.";
cDateDifference::invalidArgument="The argument \"`1`\" is not a valid `2`.";
SyntaxInformation[cDateDifference]={"ArgumentsPattern"->{_,_,_.}};

Begin["`Private`"];

(* convert units to numbers for sorting *)
cUnitValue[type_String|Quantity[_,type_]]:=Switch[type,
"Millennium"|"Millennia",1,
"Century"|"Centuries",2,
"Decade"|"Decades",3,
"Year"|"Years",4,
"Quarter"|"Quarters"|"QuarterYears",4.5,
"Month"|"Months",5,
"Week"|"Weeks",5.5,
"Day"|"Days",6,
"Hour"|"Hours",7,
"Minute"|"Minutes",8,
"Second"|"Seconds",9]

(* convert single/multiple units to Quantity units - raise a Message and Throw a Missing on invalid cases *)
cUnitToQuantityUnit[Quantity[1,type_]]:=type
cUnitToQuantityUnit[type_String]:=Switch[type,
"Second"|"Seconds","Seconds",
"Minute"|"Minutes","Minutes",
"Hour"|"Hours","Hours",
"Day"|"Days","Days",
"Week"|"Weeks","Weeks",
"Month"|"Months","Months",
"Quarter"|"Quarters"|"QuarterYears","QuarterYears",
"Year"|"Years","Years",
"Decade"|"Decades","Decades",
"Millennium"|"Millennia","Millennia",
"Century"|"Centuries","Centuries",
_,Message[cDateDifference::invalidArgument,type,"increment"];Throw[Missing["Increment"],1]]

(* reutrn start difference in float precision with it's corresponding unit as pair *)
cDateDifferenceCore[start:_Integer|_Real|{Repeated[_Integer|_Real]},end:_Integer|_Real|{Repeated[_Integer|_Real]},type_String|{type_String}|Quantity[1,type_]]:=Switch[type,
"Second"|"Seconds",{N[end]-start,"Seconds"},
"Minute"|"Minutes",{(end-start)/60.,"Minutes"},
"Hour"|"Hours",{(end-start)/3600.,"Hours"},
"Day"|"Days",{(end-start)/86400.,"Days"},
"Week"|"Weeks",{(end-start)/604800.,"Weeks"},
"Month"|"Months",{monthDifference[start,end],"Months"},
"Quarter"|"Quarters"|"QuarterYears",{quarterDifference[start,end],"QuarterYears"},
"Year"|"Years",{yearDifference[start,end],"Years"},
"Decade"|"Decades",{yearDifference[start,end]/10.,"Decades"},
"Century"|"Centuries",{yearDifference[start,end]/100.,"Centuries"},
"Millennium"|"Millennia",{yearDifference[start,end]/1000.,"Millennia"},
_,Message[cDateDifference::invalidArgument,type,"type"];Throw[Missing["Increment"],1];
]

(* multiple units *)

(* since we can't have duplicate signature, NumberQ was used *)
(* cDateDifferenceCore[start_?NumberQ,end_?NumberQ,types:{Repeated[_String|Quantity[1,_],{2,Infinity}]}]:=cDateDifferenceCore[start,end,DeleteDuplicates[cUnitToQuantityUnit/@types]] *)


cDateDifferenceCore[start:_Integer|_Real,end:_Integer|_Real,types:{Repeated[_String|Quantity[1,_],{2,Infinity}]}]:=Block[{sortedTypes=SortBy[types,cUnitValue],result},
result=Rest@FoldList[cDateDifferenceIntegerCore[Last@#1,#2]&,{{start,end}},Most[sortedTypes]];
{MixedMagnitude@#1,MixedUnit@#2}&@@Transpose@Append[Take[result,All,2],cDateDifferenceCore[result[[-1,3,1]],end,Last[sortedTypes]]]]

(* reutrn start difference in integer with it's corresponding unit as pair *)
(* used when multiple units is given to the cDateDifferenceCore *)
cDateDifferenceIntegerCore[{start:_Integer|_Real,end:_Integer|_Real},type_String|Quantity[1,type_]]:={#1[[1]],#2,{#1[[2]],end}}&@@Switch[type,
"Second"|"Seconds",{QuotientRemainder[end-start,1]*{1,-1}+{0,end},"Seconds"},
"Minute"|"Minutes",{QuotientRemainder[end-start,60]*{1,-1}+{0,end},"Minutes"},
"Hour"|"Hours",{QuotientRemainder[end-start,3600]*{1,-1}+{0,end},"Hours"},
"Day"|"Days",{QuotientRemainder[end-start,86400]*{1,-1}+{0,end},"Days"},
"Week"|"Weeks",{QuotientRemainder[end-start,604800]*{1,-1}+{0,end},"Weeks"},

"Month"|"Months",{Block[{startDate=DateList[start],endDate=DateList[end],result},
result=(First[endDate]-First[startDate])*12+(endDate[[2]]-startDate[[2]]);

(* Without Zero *)
If[Sign[First@startDate]!=Sign[First@endDate],result-=12;];

If[cAddMonth[startDate,result]>end,result-=1];
{result,AbsoluteTime[startDate+{0,result,0,0,0,0}]}],"Months"},

"Quarter"|"Quarters"|"QuarterYears",{Block[{startDate=DateList[start],endDate=DateList[end],result},
result=Quotient[(First[endDate]-First[startDate])*12+(endDate[[2]]-startDate[[2]]),3];

(* Without Zero *)
If[Sign[First@startDate]!=Sign[First@endDate],result-=4;];

If[cAddMonth[startDate,result*3]>end,result-=1];
{result,AbsoluteTime[startDate+{0,result*3,0,0,0,0}]}],"QuarterYears"},

"Year"|"Years",{Block[{startDate=DateList[start],endDate=DateList[end],result},
result=First[endDate]-First[startDate];

(* Without Zero *)
If[Sign[First@startDate]!=Sign[First@endDate],result-=1;];

If[cAddYear[startDate,result]>end,result-=1];
{result,AbsoluteTime[startDate+{result,0,0,0,0,0}]}],"Years"},

"Decade"|"Decades",{Block[{startDate=DateList[start],endDate=DateList[end],result},
result=Quotient[First[endDate]-First[startDate],10];
If[cAddYear[startDate,10*result]>end,result-=1];
{result,AbsoluteTime[startDate+{10*result,0,0,0,0,0}]}],"Decades"},

"Century"|"Centuries",{Block[{startDate=DateList[start],endDate=DateList[end],result},
result=Quotient[First[endDate]-First[startDate],100];
If[cAddYear[startDate,100*result]>end,result-=1];
{result,AbsoluteTime[startDate+{100*result,0,0,0,0,0}]}],"Centuries"},

"Millennium"|"Millennia",{Block[{startDate=DateList[start],endDate=DateList[end],result},
result=Quotient[First[endDate]-First[startDate],1000];
If[cAddYear[startDate,1000*result]>end,result-=1];
{result,AbsoluteTime[startDate+{1000*result,0,0,0,0,0}]}],"Millennia"}
]

(* main form - receive start and end start as number *)
cDateDifference[start:_Integer|_Real,end:_Integer|_Real,Optional[type_String|Quantity[1,type_],"Day"]]:=Catch[Quantity@@cDateDifferenceCore[start,end,type],_,Defer@cDateDifference[start,end,type]&]

(* first/second argument needs conversion *)
cDateDifference[start:_DateObject|_TimeObject|_String|{Repeated[_Integer|_Real,{1,6}]},end:_Integer|_Real,type:_String|_Quantity|{Repeated[_String|_Quantity]}:"Day"]:=Check[Quantity@@cDateDifferenceCore[AbsoluteTime[start],end,type],Defer@cDateDifference[start,end,type]]
cDateDifference[start:_Integer|_Real,end:_DateObject|_TimeObject|_String|{Repeated[_Integer|_Real,{1,6}]},type:_String|_Quantity|{Repeated[_String|_Quantity]}:"Day"]:=Check[Quantity@@cDateDifferenceCore[start,AbsoluteTime[end],type],Defer@cDateDifference[start,end,type]]
cDateDifference[start:_DateObject|_TimeObject|_String|{Repeated[_Integer|_Real,{1,6}]},end:_DateObject|_TimeObject|_String|{Repeated[_Integer|_Real,{1,6}]},type:_String|_Quantity|{Repeated[_String|_Quantity]}:"Day"]:=Check[Quantity@@cDateDifferenceCore[AbsoluteTime@start,AbsoluteTime@end,type],Defer@cDateDifference[start,end,type]]

(* unit wrapped in a list *)
cDateDifference[start:_Integer|_Real,end:_Integer|_Real,type:{type_String|Quantity[1,type_]}]:=Check[Quantity@@cDateDifferenceCore[start,end,type],Defer@cDateDifference[start,end,type]]

(* multiple units *)
cDateDifference[start:_Integer|_Real,end:_Integer|_Real,types:{Repeated[_String|Quantity[1,_],{2,Infinity}]}]:=Catch[Quantity@@cDateDifferenceCore[start,end,DeleteDuplicates[cUnitToQuantityUnit/@types]],_,Defer@cDateDifference[start,end,types]&]

(* single,multiple *)
cDateDifference[start:_Integer|_Real|_DateObject|_TimeObject|_String|{Repeated[_Integer|_Real,{1,6}]},ends:{Repeated[_Integer|_Real|_DateObject|_TimeObject|_String|{Repeated[_Integer|_Real,{1,6}]}]},type:_String|_Quantity]:=Catch[Quantity@@@Thread@cDateDifferenceCore[AbsoluteTime@start,AbsoluteTime/@ends,type],_,Defer@cDateDifference[start,ends,type]&]

(* multiple, single *)
cDateDifference[starts:{Repeated[_Integer|_Real|_DateObject|_TimeObject|_String|{Repeated[_Integer|_Real,{1,6}]}]},end:_Integer|_Real|_DateObject|_TimeObject|_String|{Repeated[_Integer|_Real,{1,6}]},type:_String|_Quantity]:=Catch[Quantity@@@Thread@cDateDifferenceCore[AbsoluteTime/@starts,AbsoluteTime@end,type],_,Defer@cDateDifference[starts,end,type]&]

(* multiple, multiple *)
cDateDifference[starts:{Repeated[_Integer|_Real|_DateObject|_TimeObject|_String|{Repeated[_Integer|_Real,{1,6}]}]},ends:{Repeated[_Integer|_Real|_DateObject|_TimeObject|_String|{Repeated[_Integer|_Real,{1,6}]}]},type:_String|_Quantity]:=Catch[Quantity@@@Thread@cDateDifferenceCore[AbsoluteTime/@starts,AbsoluteTime/@ends,type],_,Defer@cDateDifference[starts,ends,type]&]

(* raise a Message for invalid arguments *)
cDateDifference[start:Except[_Integer|_Real|_DateObject|_TimeObject|_String|{Repeated[_Integer|_Real|_DateObject|_TimeObject|_String]}],_,_]/;Message[cDateDifference::invalidArgument,start,"start start/list of start dates"]=False;
cDateDifference[_,end:Except[_Integer|_Real|_DateObject|_TimeObject|_String|{Repeated[_Integer|_Real|_DateObject|_TimeObject|_String]}],_]/;Message[cDateDifference::invalidArgument,end,"end start/list of end dates"]=False;
cDateDifference[_,_,type:Except[_String|Quantity[1,_]|{Repeated[_String|Quantity[1,_]]}]]/;Message[cDateDifference::invalidArgument,type,"step/list of step"]=False;

End[];



(* ::Subsection:: *)
(*cDayNameIndex, cDayName*)


cDayName::usage="Alternative to Mathematica built-in DayName.";
cDayName::invalidArgument="The argument `1` is not a valid day/list of days.";
SyntaxInformation[cDayName]={"ArgumentsPattern"->{_}};

Begin["`Private`"];

(* return day index in the list of day names from first week of 1900 *)
cDayNameIndex[start:_Integer|_Real|{Repeated[_Integer|_Real]}]:=Mod[Quotient[start,86400],7]+1

(* return DayName , is Listable *)
cDayNameValue[start:_Integer|_Real|{Repeated[_Integer|_Real]}]:={Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Sunday}[[Mod[Quotient[start,86400],7]+1]]

(* single or list of dates *)
cDayName[start:_Integer|_Real|{Repeated[_Integer|_Real,{7,Infinity}]}]:=cDayNameValue[start]

(* start argument needs conversion *)
cDayName[start:_DateObject|_String|{Repeated[_Integer|_Real,{1,6}]}]:=cDayNameValue@AbsoluteTime[start]

(* list of start needs conversion *)
cDayName[dates:{Repeated[{Repeated[_Integer|_Real,{1,6}]}|_DateObject|_String]}]:=cDayNameValue[AbsoluteTime/@dates]

(* raise Message for invalid arguments *)
cDayName[start:Except[_Integer|_Real|_DateObject|_String|{Repeated[_Integer|_Real,{1,6}]}|{Repeated[_Integer|_Real,{7,Infinity}]}|{Repeated[{Repeated[_Integer|_Real,{1,6}]}|_DateObject|_String]}]]/;Message[cDayName::invalidArgument,start,"start"]=False;

End[];



(* ::Subsection:: *)
(*cDatePlus*)


(* doesn not support real durations *)
cDatePlus::usage="Alternative to Mathematica built-in DatePlus.";
cDatePlus::invalidArgument="The argument `1` is not a valid argument for the `2`.";
SyntaxInformation[cDatePlus]={"ArgumentsPattern"->{_,_.}};

Begin["`Private`"];

(* convert Quantity to a pair *)
cQuantityToPair[Quantity[duration_,type:"Femtoseconds"|"Picoseconds"|"Nanoseconds"]]={0,"Second"};
cQuantityToPair[Quantity[duration_,type_]]:={duration,Switch[type,
"Millisecond"|"Milliseconds","Millisecond",
"Second"|"Seconds","Second",
"Minute"|"Minutes","Minute",
"Hour"|"Hours","Hour",
"Day"|"Days","Day",
"Week"|"Weeks","Week",
"Month"|"Months","Month",
"Quarter"|"Quarters"|"QuarterYears","Quarter",
"Year"|"Years","Year",
"Decade"|"Decades","Decade",
"Century"|"Centuries","Century",
"Millennium"|"Millennia","Millennium",
_,Message[cDatePlus::invalidArgument,type,"step"]
]}

(* convert units to a single base - used when multiple units is give to cDatePlus *)
cConvertUnit[{duration:_Integer|_Real,type:_String|_Symbol}|Quantity[duration_,type_]]:=Switch[type,
"Femtoseconds"|"Picoseconds"|"Nanoseconds",{0,"Second"},
"Millisecond"|"Milliseconds",{duration*10^-3,"Second"},
"Second"|"Seconds",{duration,"Second"},
"Minute"|"Minutes",{duration*60,"Second"},
"Hour"|"Hours",{duration*3600,"Second"},
"Day"|"Days",{duration*86400,"Second"},
"Week"|"Weeks",{duration*604800,"Second"},
"Month"|"Months",{duration,"Month"},
"Quarter"|"Quarters"|"QuarterYears",{duration*3,"Month"},
"Year"|"Years",{duration*12,"Month"},
"Decade"|"Decades",{120*duration,"Month"},
"Century"|"Centuries",{1200*duration,"Month"},
"Millennium"|"Millennia",{12000*duration,"Month"},
_,{duration,type}
]

(* merge sum-zero/zero units from list *)
cMergeUnits[units:{Repeated[{_Integer|_Real,_String|_Symbol}|_Quantity]}]:=DeleteCases[Reverse@*List@@@Normal@GroupBy[cConvertUnit/@units,Last->First,Total],{0|0.,_},{1}]

(* used to show minimum precision in when ouputing DateList format *)
cCalculateDateLength[(type:_Integer|_Real|_Symbol|_String)|{_,type:_String|_Symbol}|Quantity[_,type_]]:=Switch[type,
"Hour"|"Hours",4,
"Minute"|"Minutes",5,
"Second"|"Seconds",6,
_,0
]

cCalculateDateLength[units:{Repeated[{_Integer|_Real,_String|_Symbol}|_Quantity]}]:=Max[cCalculateDateLength/@units[[All,2]]]

(* used to find nth weekday/weekend day - used in cDatePlusNest *)
(* duration must be positive, the only call is with Abs[duration] - used in cDatePlusNest *)
cDatePlusNestCore[plainList_List,start_,1]:=start+86400*plainList[[cDayNameIndex[start]]]
cDatePlusNestCore[plainList_List,start_,duration_Integer]:=start+86400*Block[{offset=cDayNameIndex[start]},Nest[#+plainList[[Mod[#,7,1]]]&,offset,duration]-offset]

(* support negative list for nth weekday/weekend backward *)
cDatePlusNest[positiveList_List,negativeList_List,start_,duration_Integer]:=cDatePlusNestCore[If[Positive@duration,positiveList,negativeList],start,Abs[duration]]

(* find nth sunday/monday/... *)
cAddDayWithPattern[start_,1,pattPositive_List,pattNegative_List]:=start+86400.*pattPositive[[cDayNameIndex[start]]]
cAddDayWithPattern[start_,-1,pattPositive_List,pattNegative_List]:=start+86400.*pattNegative[[cDayNameIndex[start]]]
cAddDayWithPattern[start_,duration_,pattPositive_List,pattNegative_List]:=start+86400.*((duration-Sign[duration])*7.+If[Positive[duration],pattPositive,pattNegative][[cDayNameIndex[start]]])

(* zero duration reutrn the input *)
cDatePlusValue[start:_Integer|_Real|{Repeated[_Integer|_Real]},{0|.0,_String|_Symbol}]:=start

(* Map cDatePlusValue for units that are not Listable *)
cDatePlusValue[starts:{Repeated[_Integer|_Real]},step:{_,Except["Second"|"Minute"|"Hour"|"Day"|Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday]}]:=Thread[Unevaluated@cDatePlusValue[starts,step],List,{1}]

(* general function used for cDatePlus *)
cDatePlusValue[start:_Integer|_Real|{Repeated[_Integer|_Real]},{duration:_Integer|_Real,type:_String|_Symbol}]:=
Switch[type,
"Second"|"Seconds",start+duration,
"Minute"|"Minutes",start+60.*duration,
"Hour"|"Hours",start+3600.*duration,
"Day"|"Days",start+86400.*duration,
"Week"|"Weeks",start+604800.*duration,
Monday,cAddDayWithPattern[start,duration,{7,6,5,4,3,2,1},{-7,-1,-2,-3,-4,-5,-6}],
Tuesday,cAddDayWithPattern[start,duration,{1,7,6,5,4,3,2},{-6,-7,-1,-2,-3,-4,-5}],
Wednesday,cAddDayWithPattern[start,duration,{2,1,7,6,5,4,3},{-5,-6,-7,-1,-2,-3,-4}],
Thursday,cAddDayWithPattern[start,duration,{3,2,1,7,6,5,4},{-4,-5,-6,-7,-1,-2,-3}],
Friday,cAddDayWithPattern[start,duration,{4,3,2,1,7,6,5},{-3,-4,-5,-6,-7,-1,-2}],
Saturday,cAddDayWithPattern[start,duration,{5,4,3,2,1,7,6},{-2,-3,-4,-5,-6,-7,-1}],
Sunday,cAddDayWithPattern[start,duration,{6,5,4,3,2,1,7},{-1,-2,-3,-4,-5,-6,-7}],

"BusinessDay",Nest[Block[{temp=#+(If[Positive[duration],{1,1,1,1,3,2,1},{-3,-1,-1,-1,-1,-1,-2}][[cDayNameIndex[#]]])*86400},If[cHolidayQ[temp],cDatePlus[temp,{Sign[duration],"BusinessDay"}],temp]]&,start,Abs[duration]],

"Weekend"|"WeekendDay",cDatePlusNest[{5,4,3,2,1,1,6},{-1,-2,-3,-4,-5,-6,-1},start,duration],
"Weekday",cDatePlusNest[{1,1,1,1,3,2,1},{-3,-1,-1,-1,-1,-1,-2},start,duration],

(* unitStep are for negative durations *)
"BeginningOfMonth"|"MonthFirstDay",AbsoluteTime[DateList[start-UnitStep[-duration]*86400.]*{1,1,0,1,1,1}+{0,duration+UnitStep[-duration],1,0,0,0}],
"EndOfMonth"|"MonthLastDay",AbsoluteTime[DateList[Floor[start,86400.]+UnitStep[duration]*86400.]*{1,1,0,0,0,0}+{0,duration+UnitStep[-duration],0,0,0,0}]+Mod[start,86400.],
"Month"|"Months",cAddMonth[DateList[start],duration],
"Quarter"|"Quarters"|"QuarterYears",cAddMonth[DateList[start],duration*3],
"Year"|"Years",cAddYear[DateList[start],duration],
"Decade"|"Decades",cAddYear[DateList[start],duration*10],
"Century"|"Centuries",cAddYear[DateList[start],duration*100],
"Millennium"|"Millennia",cAddYear[DateList[start],duration*1000],

_,Message[cDatePlus::invalidArgument,type,"step"];Throw[Missing["step",type],1]]

(* cDatePlusValue with multiple units *)
cDatePlusValue[start:_Integer|_Real,steps:{Repeated[{_Integer|_Real,_String|_Symbol}|_Quantity]}]:=Catch[Fold[cDatePlusValue[#1,#2]&,start,steps],_,Throw[Missing["step"],1]&]

cConvertResultFrom[result:_Integer|_Real,base:_Integer|_Real|_String|_DateObject|_TimeObject|{Repeated[_Integer|_Real,{1,6}]},step:{_Integer|_Real,_String|_Symbol}]:=Switch[Head[base],
Integer|Real,result,
List,DateListExtended[result,Max[Length[base],cCalculateDateLength[step]]],
DateObject,DateObject@DateListExtended[result,Max[Length[First@base],cCalculateDateLength[step]]],
TimeObject,TimeObject@Take[DateList@result,{4,Max[3+Length@First[base],cCalculateDateLength[step]]}],
String,DateString@result
]

(* raise error for multiple units sum to 0 - put above to make sure nothing wrong will go down *)
cDatePlus[start_,steps_]/;(cMergeUnits[steps]==={}):=(Message[cDatePlus::invalidArgument,steps,"step (does not increase or decrease the date)"];Defer[cDatePlus[start,steps]])

(* convert Quantity to pair *)
cDatePlus[start:_Integer|_Real|_String|_TimeObject|_DateObject|_DateInterval|{Repeated[_Integer|_Real|_DateObject|_TimeObject|_String|_DateInterval|{Repeated[_Integer|_Real,{1,6}]}]},step_Quantity]:=Check[cDatePlus[start,cQuantityToPair@step],Defer@cDatePlus[start,step]]

(* no step is provided *)
cDatePlus[start:_Integer|_Real|_String|_TimeObject|_DateObject|_DateInterval|{Repeated[_Integer|_Real|_DateObject|_TimeObject|_String|_DateInterval|{Repeated[_Integer|_Real,{1,6}]}]}]:=cDatePlus[start,{1,"Day"}]

(* single String|Symbol for second argument *)
cDatePlus[start:_Integer|_Real|_String|_TimeObject|_DateObject|_DateInterval|{Repeated[_Integer|_Real|_DateObject|_TimeObject|_String|_DateInterval|{Repeated[_Integer|_Real,{1,6}]}]},type:_String|_Symbol|{Repeated[_String|_Symbol]}]:=Check[cDatePlus[start,Thread@{1,type}],Defer@cDatePlus[start,type]]

(* number for second argument *)
cDatePlus[start:_Integer|_Real|_DateObject|_String|_DateInterval|{Repeated[_Integer|_Real,{1,6}]},duration:_Integer|_Real]:=cDatePlus[start,{duration,"Day"}]

(* return start for empty list or zero Quantity for type *)
cDatePlus[start:_Integer|_Real|_String|_TimeObject|_DateObject|_DateInterval|{Repeated[_Integer|_Real|_DateObject|_TimeObject|_String|{Repeated[_Integer|_Real,{1,6}]}]},{}|{0,_String|_Symbol}]:=start

(* convert output base on input type *)
cDatePlus[start:_Integer|_Real|_DateObject|_TimeObject|_String|{Repeated[_Integer|_Real,{1,6}]},step:{_Integer|_Real,_String|_Symbol}|{Repeated[{_Integer|_Real,_String|_Symbol}|_Quantity]}]:=Catch[Check[If[MatchQ[#,_Defer],#,Switch[Head[start],
Integer|Real,Identity[#],
List,DateListExtended[#,Max[Length[start],cCalculateDateLength[step]]],
DateObject,DateObject[DateListExtended[#,Max[Length[First@start],cCalculateDateLength[step]]]],
TimeObject,TimeObject@Take[DateList@#,{4,Max[3+Length@First[start],cCalculateDateLength[step]]}],
String,DateString[#]
]]&@cDatePlusValue[AbsoluteTime[start],step],Defer@cDatePlus[start,step]],_,Defer@cDatePlus[start,step]&]

(* cDatePlus for DateInterval *)
cDatePlus[date_DateInterval,step:{_Integer|_Real,_String|_Symbol}|{Repeated[{_Integer|_Real,_String|_Symbol}|_Quantity]}]:=Check[DateInterval@Map[FromAbsoluteTime,Map[cDatePlus[AbsoluteTime@#,step]&,start["Dates"],{2}],{2}],Defer@cDatePlus[start,step]]

(* single/multiple item in first argument - interger/real seprated for saving performance because of applying AbsoluteTime *)
cDatePlus[starts:_Integer|_Real|{Repeated[_Integer|_Real,{7,Infinity}]},step:{_Integer|_Real,_String|_Symbol}]:=Catch[cDatePlusValue[starts,step],_,Defer@cDatePlus[starts,step]&]
cDatePlus[starts:{Repeated[_Integer|_Real|_DateObject|_TimeObject|_String|{Repeated[_Integer|_Real,{1,6}]}]},step:{_Integer|_Real,_String|_Symbol}|{Repeated[{_Integer|_Real,_String|_Symbol}|_Quantity]}]:=Catch[With[{results=cDatePlusValue[AbsoluteTime/@starts,step]},Thread[Unevaluated@cConvertResultFrom[results,starts,step],List,2]],_,Defer@cDatePlus[starts,step]&]

(* raise Message for invalid arguments *)
cDatePlus[start:Except[_Integer|_Real|_String|_TimeObject|_DateObject|{Repeated[_Integer|_Real|_DateObject|_TimeObject|_String|{Repeated[_Integer|_Real,{1,6}]}]}],_]/;Message[cDatePlus::invalidArgument,start,"start"]=False;
cDatePlus[_,step:Except[_Integer|_Real|_String|_Symbol|{_Integer|_Real,_String|_Symbol}|_Quantity|{Repeated[{_Integer|_Real,_String|_Symbol}|_Quantity]}]]/;Message[cDatePlus::invalidArgument,step,"step"]=False;



End[];



(* ::Subsection:: *)
(*cDayPlus*)


cDayPlus::usage="Alternative to Mathematica built-in DayPlus.";
cDayPlus::invalidArgument="The argument `1` is not a valid argument for the `2`.";
SyntaxInformation[cDayPlus]={"ArgumentsPattern"->{_,_,_.}};

Begin["`Private`"];

(* used cDatePlus as the engine *)
cDayPlus[start:_Integer|_Real|_DateObject|_String|{Repeated[_Integer|_Real,{1,6}]},duration:_Integer|_Real,All]:=DateObject[cDatePlusValue[AbsoluteTime@start,{duration,"Day"}],"Day"]
cDayPlus[start:_Integer|_Real,duration:_Integer|_Real,type:"Day"|Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday|"Week"|"Weekday"|"Weekend"|"EndOfMonth"|"BeginningOfMonth"|"BusinessDay":"Day"]:=DateObject[cDatePlusValue[start,{duration,type}],"Day"]
cDayPlus[start:_DateObject|_String|{Repeated[_Integer|_Real,{1,6}]},duration:_Integer|_Real,type:"Day"|Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday|"Week"|"Weekday"|"Weekend"|"EndOfMonth"|"BeginningOfMonth"|"BusinessDay":"Day"]:=DateObject[cDatePlusValue[AbsoluteTime@start,{duration,type}],"Day"]

cDayPlus[dates:{Repeated[_Integer|_Real,{7,Infinity}]},duration:_Integer|_Real,type:"Day"|Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday|"Week"|"Weekday"|"Weekend"|"EndOfMonth"|"BeginningOfMonth"|"BusinessDay":"Day"]:=DateObject[#,"Day"]&/@cDatePlusValue[dates,{duration,type}]
cDayPlus[dates:{Repeated[_Integer|_Real|_DateObject|_String|{Repeated[_Integer|_Real,{1,6}]}]},duration:_Integer|_Real,type:"Day"|Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday|"Week"|"Weekday"|"Weekend"|"EndOfMonth"|"BeginningOfMonth"|"BusinessDay":"Day"]:=DateObject[#,"Day"]&/@cDatePlusValue[AbsoluteTime/@dates,{duration,type}]

(* raise Message for invalid arguments *)
cDayPlus[start:Except[_Integer|_Real|_DateObject|_String|{Repeated[_Integer|_Real,{1,6}]}|{Repeated[_Integer|_Real|_DateObject|_String|{Repeated[_Integer|_Real,{1,6}]}]}],_,_]/;Message[cDayPlus::invalidArgument,start,"start"]=False;
cDayPlus[_,duration:Except[_Integer|_Real],_:"Day"]/;Message[cDayPlus::invalidArgument,duration,"duration"]=False;
cDayPlus[_,_,type:Except["Day"|Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday|"Week"|"Weekday"|"Weekend"|"EndOfMonth"|"BeginningOfMonth"|"BusinessDay"]]/;Message[cDayPlus::invalidArgument,type,"type"]=False;

End[];



(* ::Subsection:: *)
(*FromAbsoluteTimeExtended*)


Begin["`Private`"];

(* show DateList format without zero *)
(* with DateList applied, day/month could not be 0 *)
DateListExtended[start:_Real|_Integer|_DateObject|_TimeObject|_String|{Repeated[_Integer|_Real,{1,6}]}]:=Block[{temp=DateList[start]},Switch[temp,
{_,_,_,0,0,0.},Take[temp,3],
{_,_,_,_,0,0.},Take[temp,4],
{_,_,_,_,_,0.},Take[temp,5],
_,temp
]]

DateListExtended[start:_Real|_Integer|_DateObject|_String|{Repeated[_Integer|_Real,{1,6}]},0]:=DateListExtended[start]

(* second argument is for allowing extra zeros to escape *)
DateListExtended[start:_Real|_Integer|_DateObject|_String|{Repeated[_Integer|_Real,{1,6}]},upper_Integer]:=Block[{temp=DateList[start]},Switch[temp,
{_,_,_,0,0,0.},Take[temp,Max[3,upper]],
{_,_,_,_,0,0.},Take[temp,Max[4,upper]],
{_,_,_,_,_,0.},Take[temp,Max[5,upper]],
_,temp
]]

(* convert FromAbsolute to DateObject by removing extra zeros *)
FromAbsoluteTimeExtended[start:_Real|_Integer]:=DateObject@DateListExtended[start]
FromAbsoluteTimeExtended[start:_Real|_Integer,upper_Integer]:=DateObject@DateListExtended[start,upper]
FromAbsoluteTimeExtended[start:_Real|_Integer,0]:=DateObject@DateListExtended[start]

End[];



(* ::Subsection:: *)
(*cDayRound*)


cDayRound::usage="Alternative to Mathematica built-in DayRound.";
cDayRound::invalidArgument="The argument `1` is not a valid argument for the `2`.";
SyntaxInformation[cDayRound]={"ArgumentsPattern"->{_,_.}};

Begin["`Private`"];

(* Map cDayRoundValue for not listable types *)
cDayRoundValue[dates:{Repeated[_Integer|_Real]},type:"BusinessDay"|"EndOfMonth"|"MonthLastDay"|"BeginningOfMonth"|"MonthFirstDay"|"Holiday"]:=Catch[Thread[Unevaluated@cDayRoundValue[dates,type]],_,Throw[Missing["type"],1]&]

(* date argument is a number *)
cDayRoundValue[date:_Integer|_Real|{Repeated[_Integer|_Real]},type:_Symbol|_String]:=Block[{day=cDayNameIndex[date]},
Switch[type,
Sunday,7-day,
Monday,{0,6,5,4,3,2,1}[[day]],
Tuesday,{1,0,6,5,4,3,2}[[day]],
Wednesday,{2,1,0,6,5,4,3}[[day]],
Thursday,{3,2,1,0,6,5,4}[[day]],
Friday,{4,3,2,1,0,6,5}[[day]],
Saturday,{5,4,3,2,1,0,6}[[day]],
"Weekend"|"WeekendDay",{5,4,3,2,1,0,0}[[day]],
"Weekday",{0,0,0,0,0,2,1}[[day]],

"BusinessDay",Block[{offset={0,0,0,0,0,2,1}[[day]]},Block[{temp=date+offset*86400.},If[cHolidayQ[temp],offset+1+cDayRoundValue[temp+86400.,"BusinessDay"],offset]]],
"EndOfMonth"|"MonthLastDay",Quotient[AbsoluteTime[DateList[date]*{1,1,0,0,0,0}+{0,1,0,0,0,0}]-Floor[date,86400.],86400.],
"BeginningOfMonth"|"MonthFirstDay",Quotient[AbsoluteTime[DateList[date-86400.]*{1,1,0,0,0,0}+{0,1,1,0,0,0}]-Floor[date,86400.],86400.],
"Holiday",Quotient[cNextHoliday[date]-date,86400],
_,Message[cDayRound::invalidArgument,type,"type"];Throw[Missing["type"],1]
]]

(* return 0 for empty list as type *)
cDayRoundValue[_Integer|_Real,{}]:=0

(* return list of 0 for a list of dates with empty list as type *)
cDayRoundValue[date:{Repeated[_Integer|_Real]},{}]:=ConstantArray[0,Length[date]]

(* Map cDayRoundValue for not listable types *)
cDayRoundBackwardValue[dates:{Repeated[_Integer|_Real]},type:"BusinessDay"|"EndOfMonth"|"MonthLastDay"|"BeginningOfMonth"|"MonthFirstDay"|"Holiday"]:=Catch[Thread[Unevaluated@cDayRoundBackwardValue[dates,type]],_,Throw[Missing["type"],1]&]

(* date argument is a number *)
cDayRoundBackwardValue[date:_Integer|_Real|{Repeated[_Integer|_Real]},type:_Symbol|_String]:=Block[{day=cDayNameIndex[date]},
Switch[type,
Sunday,7-day,
Monday,{0,-1,-2,-3,-4,-5,-6}[[day]],
Tuesday,{-6,0,-1,-2,-3,-4,-5}[[day]],
Wednesday,{-5,-6,0,-1,-2,-3,-4}[[day]],
Thursday,{-4,-5,-6,0,-1,-2,-3}[[day]],
Friday,{-3,-4,-5,-6,0,-1,-2}[[day]],
Saturday,{-2,-3,-4,-5,-6,0,-1}[[day]],
"Weekend"|"WeekendDay",{-1,-2,-3,-4,-5,0,0}[[day]],
"Weekday",{0,0,0,0,0,-1,-2}[[day]],

"BusinessDay",Block[{offset={0,0,0,0,0,-1,-2}[[day]],temp},temp=date+offset*86400.;If[cHolidayQ[temp],offset+1+cDayRoundValue[temp+86400.,"BusinessDay"],offset]],
"EndOfMonth"|"MonthLastDay",Quotient[AbsoluteTime[DateList[date+86400.]*{1,1,0,0,0,0}]-Floor[date,86400.],86400.],
"BeginningOfMonth"|"MonthFirstDay",Quotient[AbsoluteTime[DateList[date]*{1,1,0,0,0,0}+{0,0,1,0,0,0}]-Floor[date,86400.],86400.],
"Holiday",Quotient[cPreviousHoliday[date]-date,86400],
_,Message[cDayRound::invalidArgument,type,"type"];Throw[Missing["type",1]]
]]

(* return 0 for empty list as type *)
cDayRoundBackwardValue[_Integer|_Real,{}]=0;

(* return list of 0 for a list of dates with empty list as type *)
cDayRoundBackwardValue[date:{Repeated[_Integer|_Real]},{}]:=ConstantArray[0,Length[date]]

(* do the calculations with cDayRoundValue *)
cDayRoundCore[date:_Integer|_Real,type:_Symbol|_String]:=DateObject[Floor[date,86400.]+cDayRoundValue[date,type]*86400.,"Day"]

(* do the calculations for a list of dates *)
cDayRoundCore[dates:{Repeated[_Integer|_Real]},type:_Symbol|_String]:=With[{calculatedDates=Floor[dates,86400.]+cDayRoundValue[dates,type]*86400.},Thread@Unevaluated@DateObject[calculatedDates,"Day"]]

(* single argument *)
(*cDayRound[date:_Integer|_Real|_DateObject|_String|{Repeated[_Integer|_Real,{1,6}]}]:=DateObject[date,"Day"]*)

(* list of dates without type *) 
(*cDayRound[dates:{Repeated[_Integer|_Real,{7,Infinity}]}|{Repeated[_DateObject|_String|{Repeated[_Integer|_Real,{1,6}]}]}]:=Thread[Unevaluated@DateObject[dates,"Day"]]*)

(* date argument is a number *)
cDayRound[date:_Integer|_Real,type:_Symbol|_String:"Day"]:=Catch[cDayRoundCore[date,type],_,Defer@cDayRound[date,type]&]

(* date argument needs conversion *)
cDayRound[date:_DateObject|_String|{Repeated[_Integer|_Real,{1,6}]},type:_Symbol|_String:"Day"]:=Catch[cDayRoundCore[AbsoluteTime@date,type],_,Defer@cDayRound[date,type]&]

(* list of dates that needs conversion with type *)
cDayRound[dates:{Repeated[_DateObject|_String|{Repeated[_Integer|_Real,{1,6}]}]},type:_Symbol|_String:"Day"]:=Catch[cDayRoundCore[AbsoluteTime/@dates,type],_,Defer@cDayRound[dates,type]&]

(* list of numbers with type *)
cDayRound[dates:{Repeated[_Integer|_Real,{7,Infinity}]},type:_Symbol|_String:"Day"]:=Catch[cDayRoundCore[dates,type],_,Defer@cDayRound[dates,type]&]

(* raise Message for invalid arguments *)
cDayRound[date:Except[_Integer|_Real|_DateObject|_String|{Repeated[_Integer|_Real|_DateObject|_String|{Repeated[_Integer|_Real,{1,6}]}]}],_]/;Message[cDayRound::invalidArgument,date,"date"]=False;
cDayRound[_,type:Except[_Symbol|_String]]/;Message[cDayRound::invalidArgument,type,"type"]=False;

End[];



(* ::Subsection:: *)
(*cHolidayQ*)


Begin["`Private`"];

cHolidayQ::invalidArgument="The input argument `1` is not a valid day.";

(* cAddDayOff is a function which will be applied to offical holidays to add "Day Off" days *)
cAddDayOff[_Integer][12,31,_Integer|_Real]={12,31,101.75};
cAddDayOff[year_Integer][1,1,holidayCode:_Integer|_Real]:=Switch[cDayNameIndex@AbsoluteTime@{year,1,1},7,{1,2,holidayCode+.25},_,{1,1,holidayCode}]
cAddDayOff[year_Integer][1,1,holidayCode:_Integer|_Real]:=Switch[cDayNameIndex@AbsoluteTime@{year,1,1},7,{1,2,holidayCode+.25},_,{1,1,holidayCode}]
cAddDayOff[year_Integer][month_Integer,day_Integer,holidayCode:_Integer|_Real]:=Switch[cDayNameIndex@AbsoluteTime@{year,month,day},6,{month,day-1,holidayCode+.75},7,{month,day+1,holidayCode+.25},_,{month,day,holidayCode}]

(* populate holidays for the give year and span of months *)
(* first if is a check to wether add YYYY-12-31 base one the YYYY+1-1-1 *)
cHolidayDaysGenerate[{year_,monthRange_Span}]:=cAddDayOff[year]@@@Catenate@*List@@HoldComplete[
{If[year>=1871,{1,1,101},Nothing],If[year>=1986,{1,{15,21,20,19,18,17,16}[[cDayNameIndex@AbsoluteTime@{year,1,1}]],102},Nothing]},
{If[1885<year<1971,{2,22,201},Nothing],If[year>=1971,{2,{15,21,20,19,18,17,16}[[cDayNameIndex@AbsoluteTime@{year,2,1}]],201},Nothing]},
Nothing,
Nothing,
{If[year>=1971,{5,{29,28,27,26,25,31,30}[[cDayNameIndex@AbsoluteTime@{year,5,1}]],501},Nothing]},
{If[year>=2021,{6,19,601},Nothing]},
{If[year>=1941,{7,4,701},Nothing]},
Nothing,
{If[year>=1894,{9,{1,7,6,5,4,3,2}[[cDayNameIndex@AbsoluteTime@{year,9,1}]],901},Nothing]},
{If[1977>=year>=1971,{10,{22,28,27,26,25,24,23}[[cDayNameIndex@AbsoluteTime@{year,10,1}]],1101},Nothing],If[1936<year<1971,{12,1001},Nothing],If[year>=1971,{10,{8,14,13,12,11,10,9}[[cDayNameIndex@AbsoluteTime@{year,10,1}]],1001},Nothing]},
{If[year>=1938&&Not[1977>=year>=1971],{11,11,1101},Nothing],If[year>=1863,{11,{25,24,23,22,28,27,26}[[cDayNameIndex@AbsoluteTime@{year,11,1}]],1102},Nothing]},
{If[year>=1871,{12,25,1201},Nothing],If[year>=1870&&cDayNameIndex@AbsoluteTime@{year+1,1,1}==6,{12,31,101.75},Nothing]}
][[monthRange]]

(* filter official holidays for a specific day or month or holidayCode *)
cHolidayDaysFilter[{year_Integer,month_Integer},holidayCode_Integer]:=Cases[cHolidayDaysGenerate[{year,month;;month}],{_,_,holidayCode},{1},1]
cHolidayDaysFilter[{year_Integer,month_Integer,day_Integer,Repeated[_,{0,3}]}]:=Cases[cHolidayDaysGenerate[{year,month;;month}],{_,day,_},{1},1]

(* convert official holiday pairs generated by cHolidayDays to AbsoluteTime values *)
cHolidayDaysToAbsolute[year_][result_List]:=AbsoluteTime[{year,##}]&@@@Take[result,All,2]

(* holidayQ without checking day offs *)
cHolidayQPlain[{year_Integer,month_Integer,day_Integer}]:=Switch[month,
3|4|8,False,
1,Or[year>=1871&&day==1,year>=1986&&day=={15,21,20,19,18,17,16}[[cDayNameIndex@AbsoluteTime@{year,1,1}]]],
2,Or[1885<year<1971&&day==22,year>=1971&&day=={15,21,20,19,18,17,16}[[cDayNameIndex@AbsoluteTime@{year,2,1}]]],
5,year>=1971&&day=={29,28,27,26,25,31,30}[[cDayNameIndex@AbsoluteTime@{year,5,1}]],
6,year>=2021&&day==19,
7,year>=1941&&day==4,
9,year>=1894&&day=={1,7,6,5,4,3,2}[[cDayNameIndex@AbsoluteTime@{year,9,1}]],
10,Or[1977>=year>=1971&&day=={22,28,27,26,25,24,23}[[cDayNameIndex@AbsoluteTime@{year,10,1}]],1936<year<1971&&day==12,year>=1971&&day=={8,14,13,12,11,10,9}[[cDayNameIndex@AbsoluteTime@{year,10,1}]]],
11,Or[year>=1938&&Not[1977>=year>=1971]&&day==11,year>=1863&&day=={25,24,23,22,28,27,26}[[cDayNameIndex@AbsoluteTime@{year,11,1}]]],
12,year>=1871&&day==25,
_,False
]

(* check wether a specific day is a holiday or not - for performance reason, code base has been duplicated *)
(* there is another method for checking holidays that uses cHolidayDays with slower performance *)
cHolidayQCore[{year_Integer,month_Integer,day_Integer}]:=
Or[cHolidayQPlain[{year,month,day}]&&cDayNameIndex[AbsoluteTime@{year,month,day}]<6,
(* is monday and sunday was a official holiday *)cDayNameIndex@AbsoluteTime@{year,month,day}==1&&cHolidayQPlain@Take[DateList@{year,month,day-1},3],
(* is friday and saturday is a official holiday *)cDayNameIndex@AbsoluteTime@{year,month,day}==5&&cHolidayQPlain@Take[DateList@{year,month,day+1},3]]

(* cHolidayYear will generate the official holidays base on the filters given *)

(* cHolidayYear[2020] *)
cHolidayYear[year_Integer]:=cHolidayDaysToAbsolute[year]@cHolidayDaysGenerate[{year,;;12}]

(* cHolidayYear[2020,2] - first 2 month *)
cHolidayYear[year_Integer,endMonth_Integer?Positive]:=cHolidayDaysToAbsolute[year]@cHolidayDaysGenerate[{year,1;;endMonth}]

(* cHolidayYear[2020,-2] - last 2 month *)
cHolidayYear[year_Integer,endMonth_Integer?Negative]:=cHolidayDaysToAbsolute[year]@cHolidayDaysGenerate[{year,endMonth;;12}]

(* cHolidayYear[2020,{2}] - only second month *)
cHolidayYear[year_Integer,{singleMonth_Integer}]:=cHolidayDaysToAbsolute[year]@cHolidayDaysGenerate[{year,singleMonth;;singleMonth}]

(* cHolidayYear[2020,{2,1}] - from 2020 to 2020-2-1 *)
cHolidayYear[year_Integer,{endMonth_Integer?Positive,endDay_Integer}]:=cHolidayDaysToAbsolute[year]@Join[If[endMonth!=1,cHolidayDaysGenerate[{year,1;;endMonth-1}],{}],Cases[cHolidayDaysGenerate[{year,endMonth;;endMonth}],{_,day_,_}/;day<=endDay]]

(* cHolidayYear[2020,{-2,1}] - from 2020-10-1 to 2021 *)
cHolidayYear[year_Integer,{startMonth_Integer?Negative,startDay_Integer}]:=cHolidayDaysToAbsolute[year]@Join[Cases[cHolidayDaysGenerate[{year,startMonth;;startMonth}],{_,day_,_}/;day>=startDay],If[startMonth!=-1,cHolidayDaysGenerate[{year,startMonth+1;;12}],{}]]

(* cHolidayYear[2020,{2,3},{4,5}] - from 2020-2-3 to 2020-4-5 *)
cHolidayYear[year_Integer,{startMonth_Integer?Positive,startDay_Integer},{endMonth_Integer?Positive,endDay_Integer}]/;startMonth<=endMonth:=Intersection[cHolidayYear[year,{startMonth-13,startDay}],cHolidayYear[year,{endMonth,endDay}]]

(* cHolidayYear[2020,{2,3},{2,5}] - from 2020-2-3 to 2020-2-5 *)
cHolidayYear[year_Integer,{month_Integer,startDay_Integer},{month_Integer,endDay_Integer}]/;startDay<endDay:=cHolidayDaysToAbsolute[year]@Cases[cHolidayDaysGenerate[{year,month;;month}],{_,Alternatives@@Range[startDay,endDay],_}]

cNextHoliday[date:_Integer|_Real|_DateObject|{Repeated[_Integer|_Real,{1,6}]}]:=Quiet[Check[First@cHolidayYear[#1,{#2,#3+1},{12,31}],cNextHoliday[{#1+1,1,1,0,0,0}]]&@@DateList[date]]

cPreviousHoliday::noOfficalHolidayExist="No offical holiday exists before 1863-11-26.";
cPreviousHoliday[date:_Integer|_Real|_DateObject|{Repeated[_Integer|_Real,{1,6}]}]:=Catch[If[AbsoluteTime[date]<-1139184000,Message[cPreviousHoliday::noOfficalHolidayExist];Throw["NoOfficalHolidayExist",1]];Quiet[Check[Last@If[#2==1&&#3==1,cHolidayYear[#1-1,{1,1},{12,31}],cHolidayYear[#1,{1,1},{#2,#3-1}]],cPreviousHoliday[{#1-1,1,1,0,0,0}]]&@@DateList[date]],_,Defer@cPreviousHoliday[date]&]

(* generate official holidays for a range of start - used for cHolidayRange *)
cHolidayRangeCore[{startYear_Integer,startMonth_Integer,startDay_Integer},{endYear_Integer,endMonth_Integer,endDay_Integer}]:=Catenate@{
If[startYear==endYear,cHolidayYear[startYear,{startMonth,startDay},{endMonth,endDay}],cHolidayYear[startYear,{startMonth-13,startDay}]],
If[endYear-startYear>=2,Splice@*cHolidayYear/@Range[startYear+1,endYear-1],{}],
If[endYear>startYear,cHolidayYear[endYear,{endMonth,endDay}],{}]
}

(* return official holiday name + "Day off" days also included *)
(* if start is given, filter with cHolidayDays and use the return result to get the name *)
SyntaxInformation[cHolidayName]={"ArgumentsPattern"->{_}};

cHolidayName::invalidArgument="The input argument `1` is not a valid day.";
cHolidayName[start:_Integer|_Real|_String|_DateObject|{Repeated[_Integer|_Real,{1,6}]}]:=Block[{possibleDays=cHolidayDaysFilter[DateList@start]},If[Length@possibleDays==1,cHolidayNameCore@Take[First@possibleDays,{2,3}],Missing["Holiday"]]]

cHolidayName[start_]:=Message[cHolidayName::invalidArgument,start]

(* for real holidayCode (.25, .75) it's a "Day Off" holiday *)
cHolidayNameCore[{day_Integer,holidayCode_Real}]/;MatchQ[FractionalPart[holidayCode],.25|.75]:=If[MissingQ@#,#,#<>"'s day off"]&@cHolidayNameCore[{day,Floor[holidayCode]}]

(* for integer holidayCode, it's an offical holiday *)
cHolidayNameCore[{day_Integer,holidayCode_Integer}]:=Switch[holidayCode,
101,"New Year's Day",
102,"Martin Luther King Jr. Day",
201,"Presidents' Day",
501,"Memorial Day",
601,"Juneteenth",
701,"Independence Day",
901,"Labor Day",
1001,"Columbus Day",
1101,"Veterans Day",
1102,"Thanksgiving Day",
1201,"Christmas Day",
_,Throw[Missing["Holiday"],1]
]

(* generate official holidays for a range of start *)
cHolidayRange[start:_Integer|_Real|_DateObject|_String|{Repeated[_Integer|_Real,{1,6}]},end:_Integer|_Real|_DateObject|_String|{Repeated[_Integer|_Real,{1,6}]}]:=cHolidayRangeCore[Take[DateList[start],3],Take[DateList[end],3]]

(*
(* cHolidayQ - Method 1 - slower *)
cHolidayQ[a:_Integer|_Real|_String|{Repeated[_Integer|_Real,{1,6}]}]:=Length@cHolidayDays@DateList@a==1
cHolidayQ[a_DateObject]:=Length@cHolidayDays@Take[First[a],3]==1
*)

(* cHolidayQ - Method 2 uglier - almost 1.5 times faster than method 1 *)
cHolidayQ[date:_Integer|_Real|_String|_DateObject|{Repeated[_Integer|_Real,{1,6}]}]:=cHolidayQCore[Take[DateList[date],3]]

(* raise Message for dates without specific day *)
cHolidayQ[date:Except[_Integer|_Real|_String|_DateObject|{Repeated[_Integer|_Real,{1,6}]}]]:=(Message[cHolidayQ::invalidArgument,date];False)

End[];



(* ::Subsection:: *)
(*cDayRange*)


cDayRange::usage="Alternative to Mathematica built-in DayRange.";
cDayRange::invalidArgument="The argument `1` is not a valid argument for the `2`.";
SyntaxInformation[cDayRange]={"ArgumentsPattern"->{_,_,_.}};

Begin["`Private`"];

(* used to find every Sunday/Monday/Weekend/... *)
(* cCalculateDays[start:_Integer|_Real,days_Integer,target:{__Integer}]:=start+86400.*Sort@Catenate@Range[Mod[target-cDayNameIndex[start],7],days-1,7] *)
cCalculateDays[start:_Integer|_Real,days_Integer,target:{__Integer}]:=start+86400.*Sort@Catenate@Range[Mod[target-cDayNameIndex[start],7],days,7]

(* simplified with single target (sort is not needed) *)
(* cCalculateDays[start:_Integer|_Real,days_Integer,target_Integer]:=start+86400.*Range[Mod[target-cDayNameIndex[start],7],days-1,7] *)
cCalculateDays[start:_Integer|_Real,days_Integer,target_Integer]:=start+86400.*Range[Mod[target-cDayNameIndex[start],7],days,7]

(* used to find nth Sunday/Monday/Weekend/... *)
(* single target *)
(* cCalculateDays[start:_Integer|_Real,days_Integer,target_Integer,multiplier_Integer]:=start+86400.*Range[Mod[target-cDayNameIndex[start],Sign[multiplier]*7],Sign[multiplier]*days-Sign[multiplier],7*multiplier] *)
cCalculateDays[start:_Integer|_Real,days_Integer,target_Integer,multiplier_Integer]:=start+86400.*Range[Mod[target-cDayNameIndex[start],Sign[multiplier]*7],Sign[multiplier]*days,7*multiplier]
(* multiple target *)
cCalculateDays[start:_Integer|_Real,days_Integer,target:{__Integer},multiplier_Integer]:=start+86400.*Sort@Catenate@Range[Mod[target-cDayNameIndex[start],Sign[multiplier]*7],Sign[multiplier]*days+1,7*multiplier]

cMonthListWithDay[year_Integer,month_Integer]:=HoldComplete[31,28+First@cLeapYearValue[{year}],31,30,31,30,31,31,30,31,30,31][[month]]

(* return number of days for a given year *)
cMonthList[year_Integer]:={31,28+First@cLeapYearValue[{year}],31,30,31,30,31,31,30,31,30,31}

(* multiple year *)
cMonthList[years:{__Integer}]:={31,28+cLeapYearValue[years],31,30,31,30,31,31,30,31,30,31}

(* generate beginning of months between two start *)
cBeginningOfMonthRangeCore[removeLastMonth:True|False,{startYear_,startMonth_,startDay_,Repeated[0|0.,{0,3}]},{endYear_,endMonth_,_,Repeated[0|0.,{0,3}]},hms:Repeated[_Integer|_Real,{1,3}]:Nothing]:=AbsoluteTime/@Thread@{startYear,startMonth+Range[0+Boole[startDay!=1],(If[Sign@endYear!=Sign@startYear,endYear-1,endYear]-startYear)*12+endMonth-startMonth-Boole[removeLastMonth]],1,hms}

(* end could have extra item such as hour/minute/... *)
cBeginningOfMonthRange[{startYear_,startMonth_,startDay_},{endYear_,endMonth_,endDay_,Repeated[_Integer|_Real,{0,3}]}]:=cBeginningOfMonthRangeCore[False,{startYear,startMonth,startDay},{endYear,endMonth,endDay}]

(* start have extra item such as hour/minute/... *)
cBeginningOfMonthRange[{startYear_,startMonth_,startDay_,startHour_,startMinute_:0,startSecond_:0},{endYear_,endMonth_,endDay_}]:=cBeginningOfMonthRangeCore[endDay==1,{startYear,startMonth,startDay},{endYear,endMonth,endDay},startHour,startMinute,startSecond]

(* start and end have extra item such as hour/minute/... *)
cBeginningOfMonthRange[{startYear_,startMonth_,startDay_,startHour_,startMinute_:0,startSecond_:0},{endYear_,endMonth_,endDay_,endHour_,endMinute_:0,endSecond_:0},type_:All]:=cBeginningOfMonthRangeCore[If[endDay!=1,False,Dot[{startHour,startMinute,startSecond},{3600,60,1}]>Dot[{endHour,endMinute,endSecond},{3600,60,1}]],{startYear,startMonth,startDay},{endYear,endMonth,endDay},If[type==All,Unevaluated@Sequence[startHour,startMinute,startSecond],Nothing]]

(* generate end of months between two start *)
cEndOfMonthRangeCore[removeLastMonth:True|False,{startYear_,startMonth_,_,Repeated[0|0.,{0,3}]},{endYear_,endMonth_,endDay_,Repeated[0|0.,{0,3}]},hms:Repeated[_Integer|_Real,{0,3}]:Nothing]:=AbsoluteTime/@Thread@{startYear,startMonth+Range[1,(If[Sign@endYear!=Sign@startYear,endYear-1,endYear]-startYear)*12+endMonth-startMonth-Boole[removeLastMonth]+1],0,hms}

(* end could have extra item such as hour/minute/... *)
cEndOfMonthRange[{startYear_,startMonth_,startDay_},{endYear_,endMonth_,endDay_,Repeated[_Integer|_Real,{0,3}]}]:=cEndOfMonthRangeCore[endDay!=cMonthListWithDay[endYear,endMonth],{startYear,startMonth,startDay},{endYear,endMonth,endDay}]

(* start have extra item such as hour/minute/... *)
cEndOfMonthRange[{startYear_,startMonth_,startDay_,startHour_,startMinute_:0,startSecond_:0},{endYear_,endMonth_,endDay_}]:=cEndOfMonthRangeCore[endDay!=cMonthListWithDay[endYear,endMonth],{startYear,startMonth,startDay},{endYear,endMonth,endDay},startHour,startMinute,startSecond]

(* start and end have extra item such as hour/minute/... *)
cEndOfMonthRange[{startYear_,startMonth_,startDay_,startHour_,startMinute_:0,startSecond_:0},{endYear_,endMonth_,endDay_,endHour_,endMinute_:0,endSecond_:0},type_:All]:=cEndOfMonthRangeCore[If[endDay!=cMonthListWithDay[endYear,endMonth],True,Dot[{startHour,startMinute,startSecond},{3600,60,1}]>Dot[{endHour,endMinute,endSecond},{3600,60,1}]],{startYear,startMonth,startDay},{endYear,endMonth,endDay},If[type==All,Unevaluated@Sequence[startHour,startMinute,startSecond],Nothing]]

cDayRangeValue[start:_Integer|_Real,end:_Integer|_Real,type:_Symbol|_String]/;start>end:=Reverse@cDayRangeValue[end,start,type]

cDayRangeValue[start:_Integer|_Real,end:_Integer|_Real,type:_Symbol|_String]:=Block[{startDay=Floor[start,86400.],endDay=Ceiling[end,86400.],days},
(* days=Echo@Quotient[endDay-startDay,86400.]; *)
days=Quotient[Floor[end,86400.]-startDay,86400.];
Switch[type,
All|"Day",Range[startDay,endDay-86400.,86400.],

Monday,cCalculateDays[startDay,days,1],
Tuesday,cCalculateDays[startDay,days,2],
Wednesday,cCalculateDays[startDay,days,3],
Thursday,cCalculateDays[startDay,days,4],
Friday,cCalculateDays[startDay,days,5],
Saturday,cCalculateDays[startDay,days,6],
Sunday,cCalculateDays[startDay,days,7],

"Weekday"|"WeekDay",cCalculateDays[startDay,days,{1,2,3,4,5}],
"Weekend"|"WeekendDay",cCalculateDays[startDay,days ,{6,7}],

"BusinessDay",Complement[cDayRangeValue[start,end,"Weekday"],N@cHolidayRange[start,end]],
"Holiday",cHolidayRange[start,end],

"BeginningOfMonth"|"MonthFirstDay",cBeginningOfMonthRange[Take[DateList[start],3],Take[DateList[end],3]],
"EndOfMonth"|"MonthLastDay",cEndOfMonthRange[Take[DateList[start],3],Take[DateList[end],3]],
_,Message[cDayRange::invalidArgument,type,"type"];Defer[cDayRange[DateObject@start,DateObject@end,type]]
]]

(* start and end are number *)
cDayRange[start:_Integer|_Real,end:_Integer|_Real,type:_Symbol|_String:All]:=DateObject[#,"Day"]&/@cDayRangeValue[start,end,type]

(* start needs to be converted *)
cDayRange[start:_Integer|_Real|_DateObject|_String|{Repeated[_Integer|_Real,{1,6}]},end:_Integer|_Real|_DateObject|_String|{Repeated[_Integer|_Real,{1,6}]},type:_Symbol|_String]:=DateObject[#,"Day"]&/@cDayRangeValue[AbsoluteTime@start,AbsoluteTime@end,type]

(* raise Message for invalid arguments *)
cDayRange[start:Except[_Integer|_Real|_DateObject|_String|{Repeated[_Integer|_Real,{1,6}]}],_,_]/;Message[cDayRange::invalidArgument,start,"start"]=False;
cDayRange[_,end:Except[_Integer|_Real|_DateObject|_String|{Repeated[_Integer|_Real,{1,6}]}],_]/;Message[cDayRange::invalidArgument,end,"end"]=False;
cDayRange[_,_,type:Except[_Symbol|_String]]/;Message[cDayRange::invalidArgument,type,"type"]=False;

End[];



(* ::Subsection:: *)
(*cDayCount*)


cDayCount::usage="Alternative to Mathematica built-in DayCount.";
cDayCount::invalidArgument="The argument `1` is not a valid argument for the `2`.";
SyntaxInformation[cDayCount]={"ArgumentsPattern"->{_,_,_.}};

Begin["`Private`"];

(* calculate number of days with a start (AbsoluteTime value), number of available days, and target *)
cCalculateNumberOfDays[start:_Integer|_Real,days:_Integer,target:{__Integer}]:=Length@Catenate@Range[Mod[target-cDayNameIndex[start],7],days,7]

(* without specifying a step *)
(*cDayCount[start:_Integer|_Real|_DateObject|_String|{Repeated[_Integer|_Real,{1,6}]},end:_Integer|_Real|_DateObject|_String|{Repeated[_Integer|_Real,{1,6}]}]:=cDayCount[start,end,All]*)

(* start needs conversion *)
cDayCount[start:_DateObject|_String|{Repeated[_Integer|_Real,{1,6}]},end:_Integer|_Real|_DateObject|_String|{Repeated[_Integer|_Real,{1,6}]},type:_Symbol|_String:All]:=cDayCount[AbsoluteTime[start],end,type]

(* end needs conversion *)
cDayCount[start:_Integer|_Real|_DateObject|_String|{Repeated[_Integer|_Real,{1,6}]},end:_DateObject|_String|{Repeated[_Integer|_Real,{1,6}]},type:_Symbol|_String:All]:=cDayCount[start,AbsoluteTime[end],type]

(* start is bigger than end *)
cDayCount[start:_Integer|_Real,end:_Integer|_Real,type:_Symbol|_String]/;start>end:=-1*cDayCountCore[end,start,type]

(* start and end are numbers *)
cDayCount[start:_Integer|_Real,end:_Integer|_Real,type:_Symbol|_String:All]:=cDayCountCore[start,end,type]

(* do the calculations *)
cDayCountCore[start:_Integer|_Real,end:_Integer|_Real,type:_Symbol|_String]:=Block[{startDay=Floor[start,86400.]+86400,endDay=Floor[end,86400.],days},
days=Quotient[endDay-startDay,86400.];

Switch[type,
All|"Day",days+1,

Monday,cCalculateNumberOfDays[startDay,days,{1}],
Tuesday,cCalculateNumberOfDays[startDay,days,{2}],
Wednesday,cCalculateNumberOfDays[startDay,days,{3}],
Thursday,cCalculateNumberOfDays[startDay,days,{4}],
Friday,cCalculateNumberOfDays[startDay,days,{5}],
Saturday,cCalculateNumberOfDays[startDay,days,{6}],
Sunday,cCalculateNumberOfDays[startDay,days,{7}],

"Weekday",cCalculateNumberOfDays[startDay,days,{1,2,3,4,5}],
"Weekend",cCalculateNumberOfDays[startDay,days ,{6,7}],
"BusinessDay",Length@Complement[cDayRangeValue[startDay,end,"Weekday"],N@cHolidayRange[start,end]],
"Holiday",Length@cHolidayRange[startDay,end],
"BeginningOfMonth",Length@cBeginningOfMonthRange[Take[DateList@startDay,3],Take[DateList@end,3]],
"EndOfMonth",Length@cEndOfMonthRange[Take[DateList@startDay,3],Take[DateList@end,3]],
_,Message[cDayCount::invalidArgument,type,"type"];Defer[cDayCount[DateObject@start,DateObject@end,type]]
]]

(* raise Message for invalid arguments *)
cDayCount[start:Except[_Integer|_Real|_DateObject|_String|{Repeated[_Integer|_Real,{1,6}]}],_,_]/;Message[cDayCount::invalidArgument,start,"start"]=False;
cDayCount[_,end:Except[_Integer|_Real|_DateObject|_String|{Repeated[_Integer|_Real,{1,6}]}],_]/;Message[cDayCount::invalidArgument,end,"end"]=False;
cDayCount[_,_,type:Except[_Symbol|_String]]/;Message[cDayCount::invalidArgument,type,"type"]=False;

End[];



(* ::Subsection:: *)
(*cDayMatchQ*)


cDayMatchQ::usage="Alternative to Mathematica built-in DayMatchQ.";
cDayMatchQ::invalidArgument="The argument `1` is not a valid day.";
SyntaxInformation[cDayMatchQ]={"ArgumentsPattern"->{_,_}};

Begin["`Private`"];

(* day argument is a number *)
cDayMatchQ[day:_Integer|_Real,type:_Symbol|_String]:=Switch[type,
Monday,cDayNameIndex[day]==1,
Tuesday,cDayNameIndex[day]==2,
Wednesday,cDayNameIndex[day]==3,
Thursday,cDayNameIndex[day]==4,
Friday,cDayNameIndex[day]==5,
Saturday,cDayNameIndex[day]==6,
Sunday,cDayNameIndex[day]==7,

"Weekday",cDayNameIndex[day]<6,
"Weekend",cDayNameIndex[day]>5,
"BusinessDay",cDayNameIndex[day]<6&&!cHolidayQ[day],
"Holiday",cHolidayQ[day],

"EndOfMonth",AbsoluteTime[DateList[day]*{1,1,0,1,1,1}+{0,1,0,0,0,0}]==day,
"BeginningOfMonth",DateList[day][[3]]==1,
_,False
]

(* day argument needs conversion *)
cDayMatchQ[day:_DateObject|_String|{Repeated[_Integer|_Real,{1,6}]},type:_Symbol|_String]:=cDayMatchQ[AbsoluteTime[day],type]

(* for invalid arguments raise Message and return False *)
cDayMatchQ[day:Except[_Integer|_Real|_DateObject|_String|{Repeated[_Integer|_Real,{1,6}]}],_]:=(Message[cDayMatchQ::invalidArgument,day,"day"];False);
cDayMatchQ[_,type:Except[_Symbol|_String]]:=(Message[cDayMatchQ::invalidArgument,type,"type"];False); 

End[];



(* ::Subsection:: *)
(*cBusinessDayQ*)


cBusinessDayQ::usage="Alternative to Mathematica built-in BusinessDayQ.";
cBusinessDayQ::invalidArgument="The argument `1` is not a valid day.";
SyntaxInformation[cBusinessDayQ]={"ArgumentsPattern"->{_}};

Begin["`Private`"];

(* day argument is a number *)
cBusinessDayQ[day:_Integer|_Real]:=And[cDayNameIndex[day]<6,!cHolidayQ[day]]

(* day argument needs conversion *)
cBusinessDayQ[day:_DateObject|_String|{Repeated[_Integer|_Real,{1,6}]}]:=cBusinessDayQ@AbsoluteTime[day]

(* return False for invalid arguments *)
cBusinessDayQ[start:Except[_DateObject|_String|{Repeated[_Integer|_Real,{1,6}]}]]:=(Message[cBusinessDayQ::invalidArgument,start];False);

End[];



(* ::Subsection:: *)
(*cDateRange*)


cDateRange::usage="Alternative to Mathematica built-in DateRange.";
cDateRange::invalidArgument="The argument `1` is not a valid argument for the `2`.";
SyntaxInformation[cDateRange]={"ArgumentsPattern"->{_,_,_.}};

Begin["`Private`"];

(* step is not given *)
cDateRange[start_,end_]:=cDateRange[start,end,{1,"Day"}]

(* single quantity *)
cDateRange[start:_Integer|_Real|_TimeObject|_String|_DateObject|{Repeated[_Integer|_Real,{1,6}]},end:_Integer|_Real|_TimeObject|_String|_DateObject|{Repeated[_Integer|_Real,{1,6}]},type:_String|_Symbol]:=cDateRange[start,end,{1,type}]

(* number as quantity *)
cDateRange[start:_Integer|_Real|_TimeObject|_String|_DateObject|{Repeated[_Integer|_Real,{1,6}]},end:_Integer|_Real|_TimeObject|_String|_DateObject|{Repeated[_Integer|_Real,{1,6}]},duration:_Integer|_Real]:=cDateRange[start,end,{duration,"Day"}]

(* zero increment *)
cDateRange[start:_Integer|_Real|_TimeObject|_String|_DateObject|{Repeated[_Integer|_Real,{1,6}]},end:_Integer|_Real|_TimeObject|_String|_DateObject|{Repeated[_Integer|_Real,{1,6}]},step:{0|.0,_String|_Symbol}|Quantity[0|.0,_]]:=Defer@cDateRange[start,end,step]

(* convert output base on the input *)
cDateRange[start:_DateObject|_TimeObject|_String|{Repeated[_Integer|_Real,{1,6}]},end:_Integer|_Real|_DateObject|_TimeObject|_String|{Repeated[_Integer|_Real,{1,6}]},step:{_Integer|_Real,_String|_Symbol}|_Quantity|{Repeated[{_Integer|_Real,_String|_Symbol}|_Quantity]}]:=Map[Switch[Head[start],
List,DateList,
DateObject,FromAbsoluteTimeExtended,
TimeObject,TimeObject[Take[DateList[#],{4,6}]]&,
String,DateString
],cDateRange[AbsoluteTime[start],end,step]]

(* end needs conversion *)
cDateRange[start:_Integer|_Real,end:_DateObject|_TimeObject|_String|{Repeated[_Integer|_Real,{1,6}]},step:{_Integer|_Real,_String|_Symbol}|_Quantity|{Repeated[{_Integer|_Real,_String|_Symbol}|_Quantity]}]:=cDateRange[start,AbsoluteTime[end],step]

(* start argument is bigger than end argument for positive steps *)
cDateRange[start:_Integer|_Real,end:_Integer|_Real,{duration:_Integer|_Real,_String|_Symbol}|Quantity[duration_,_]]/;end>start&&Negative[duration]={};

(* end argument is bigger than start argument for negative steps *)
cDateRange[start:_Integer|_Real,end:_Integer|_Real,{duration:_Integer|_Real,_String|_Symbol}|Quantity[duration_,_]]/;start>end&&Positive[duration]={};

(* special case for "BeginningOfMonth" *)
cDateRange[start:_Integer|_Real,end:_Integer|_Real,{duration:_Integer|_Real,"BeginningOfMonth"}]:=If[start>end,AbsoluteTime/@(Reverse[cBeginningOfMonthRange[DateList@end,DateList@start]][[;;;;Abs@duration]]),AbsoluteTime/@cBeginningOfMonthRange[DateList@start,DateList@end][[;;;;duration]]]

(* special case for "EndOfMonth" *)
cDateRange[start:_Integer|_Real,end:_Integer|_Real,{duration:_Integer|_Real,"EndOfMonth"}]:=If[start>end,AbsoluteTime/@(Reverse[cEndOfMonthRange[DateList@end,DateList@start]][[;;;;Abs@duration]]),AbsoluteTime/@cEndOfMonthRange[DateList@start,DateList@end][[;;;;duration]]]

(* start and end are numbers *)
cDateRange[start:_Integer|_Real,end:_Integer|_Real,step:{duration:_Integer|_Real,type:_String|_Symbol}|Quantity[duration_,type_]]:=Switch[type,
"Second"|"Seconds",Range[start,end,N@duration],
"Minute"|"Minutes",Range[start,end,duration*60.],
"Hour"|"Hours",Range[start,end,duration*3600.],
"Day"|"Days",Range[start,end,duration*86400.],

Monday,cCalculateDays[start,Quotient[Abs[end-start],86400],1,duration],
Tuesday,cCalculateDays[start,Quotient[Abs[end-start],86400],2,duration],
Wednesday,cCalculateDays[start,Quotient[Abs[end-start],86400],3,duration],
Thursday,cCalculateDays[start,Quotient[Abs[end-start],86400],4,duration],
Friday,cCalculateDays[start,Quotient[Abs[end-start],86400],5,duration],
Saturday,cCalculateDays[start,Quotient[Abs[end-start],86400],6,duration],
Sunday,cCalculateDays[start,Quotient[Abs[end-start],86400],7,duration],

"Weekend",Block[{durationSign=Sign[duration]},start+86400*SortBy[Catenate@Range[Mod[{6,7}-cDayNameIndex[start],durationSign*7],durationSign*Quotient[end-start,durationSign*86400],durationSign*7],Abs][[;;;;Abs[duration]]]],
"Weekday",Block[{durationSign=Sign[duration]},start+86400*SortBy[Catenate@Range[Mod[{1,2,3,4,5}-cDayNameIndex[start],durationSign*7],durationSign*Quotient[end-start,durationSign*86400],durationSign*7],Abs][[;;;;Abs[duration]]]],
"BusinessDay",NestWhileList[cDatePlus[#,step]&,start+86400*If[Positive[duration],cDayRoundValue[start,type],cDayRoundBackwardValue[start,type]],If[Positive[duration],LessEqualThan,GreaterEqualThan][end],1,Infinity,-1],
"Week"|"Weeks",Range[start,end,duration*604800.],

"Month"|"Months",    AbsoluteTime/@Most@NestWhileList[cAddMonthList[#,First[step]]&,DateList[start],If[Positive[duration],LessEqualThan,GreaterEqualThan][end]@*AbsoluteTime],
"Quarter"|"Quarters",AbsoluteTime/@Most@NestWhileList[cAddMonthList[#,First[step]*3]&,DateList[start],If[Positive[duration],LessEqualThan,GreaterEqualThan][end]@*AbsoluteTime],

"Year"|"Years",         AbsoluteTime/@Most@NestWhileList[cAddYearList[#,First[step]]&,DateList[start],If[Positive[duration],LessEqualThan,GreaterEqualThan][end]@*AbsoluteTime],
"Decade"|"Decades",     AbsoluteTime/@Most@NestWhileList[cAddYearList[#,First[step]*10]&,DateList[start],If[Positive[duration],LessEqualThan,GreaterEqualThan][end]@*AbsoluteTime],
"Century"|"Centuries",  AbsoluteTime/@Most@NestWhileList[cAddYearList[#,First[step]*100]&,DateList[start],If[Positive[duration],LessEqualThan,GreaterEqualThan][end]@*AbsoluteTime],
"Millennium"|"Millennia",AbsoluteTime/@Most@NestWhileList[cAddYearList[#,First[step]*1000]&,DateList[start],If[Positive[duration],LessEqualThan,GreaterEqualThan][end]@*AbsoluteTime],


_,Message[cDateRange::invalidArgument,step,"step"];Defer[cDateRange[start,end,step]]
]

(* multiple steps *)
cDateRange[start:_Integer|_Real,end:_Integer|_Real,step:{Repeated[{_Integer|_Real,_String|_Symbol}|_Quantity]}]:=NestWhileList[cDatePlusValue[#,step]&,start,end<=#<=start||start<=#<=end&,1,Infinity,-1]

(* raise Message for invalid arguments *)
cDateRange[start:Except[_Integer|_Real|_DateObject|_TimeObject|_String|{Repeated[_Integer|_Real,{1,6}]}],_,_]/;Message[cDateRange::invalidArgument,start,"start"]=False;
cDateRange[_,end:Except[_Integer|_Real|_DateObject|_TimeObject|_String|{Repeated[_Integer|_Real,{1,6}]}],_]/;Message[cDateRange::invalidArgument,end,"end"]=False;
cDateRange[_,_,step:Except[_String|_Symbol|{_Integer|_Real,_String|_Symbol}|_Quantity|{Repeated[{_Integer|_Real,_String|_Symbol}|_Quantity]}]]/;Message[cDateRange::invalidArgument,step,"step"]=False;

End[];



(* ::Subsection:: *)
(*cDateWithinQ*)


cDateWithinQ::usage="Alternative to Mathematica built-in DateWithinQ.";
cDateWithinQ::invalidArgument="The argument `1` is not a valid argument for `2`.";
SyntaxInformation[cDateWithinQ]={"ArgumentsPattern"->{_,_}};

(*
precision -> type
1   "Millennium"
1.1 "MillenniumBeginning01"
2   "Century"
2.1 "CenturyBeginning01"
3   "Decade"
4   "Year"
4.5 "Quarter"
5   "Month"
5.5 "Week"
5.75"WeekBeginningSunday"
6   "Day"
7   "Hour"
8   "Minute"
9   "Second"
10  "Instant"
*)

Begin["`Private`"];

(* return start precision of argument *)
cDatePrecision[start:_String|_DateObject|_TimeObject|{Repeated[_Integer|_Real,{1,6}]}]:=Switch[start,
_String,Switch[FromDateString[start][[2]],"Year",4,"Month",5,"Day",6,"Minute",8,"Instant",10],
_DateObject,Switch[start[[2]],"Millennium",1,"MillenniumBeginning01",1.1,"Century",2,"CenturyBeginning01",2.1,"Decade",3,"Year",4,"Quarter",4.5,"Month",5,"Week",5.5,"WeekBeginningSunday",5.75,"Day",6,"Hour",7,"Minute",8,"Second",9,"Instant",10],
_List,Length[start]+3,
_TimeObject,Switch[start[[2]],"Hour",7,"Minute",8,"Second",9,"Instant",10]
]

(* check whether the second argument is between lowerRange and upperRange *)
cDateWhitinComparison[{lowerRange:_Integer|_Real,upperRange:_Integer|_Real},{upperValue:_Integer|_Real,lowerValue:_Integer|_Real}]:=lowerRange<=lowerValue<=upperRange&&lowerRange<=upperValue<=upperRange

(* convert start to the type argument precision *)
cCustomDateList[start:_Integer|_Real|_DateObject|_String|{Repeated[_Integer|_Real,{1,6}]},type:_String|_Integer|_Real]:=Switch[type,
"Millennium"|1,Quotient[First@DateList[start],1000]+1,
"MillenniumBeginning01"|1.1,Quotient[First@DateList[start]-1,1000]+1,
"Century"|2,Quotient[First@DateList[start],100]+1,
"CenturyBeginning01"|2.1,Quotient[First@DateList[start]-1,100]+1,
"Decade"|3,Quotient[First@DateList[start],10]+1,
"Year"|4,First@DateList[start],
"Quarter"|4.5,{#1,Quotient[#2,3,1]+1}&@@DateList[start],
"Month"|5,Take[DateList[start],2],
"Week"|5.5,Quotient[AbsoluteTime[start],604800.]+1,
"WeekBeginningSunday"|5.75,{Quotient[AbsoluteTime[start]+86400.,604800.]+1},
"Day"|6,Quotient[AbsoluteTime[start],86400.],
"Hour"|7,Quotient[AbsoluteTime[start],3600.],
"Minute"|8,Quotient[AbsoluteTime[start],60.],
"Second"|9,Floor@AbsoluteTime[start],
"Instant"|10,DateList[start]
]

(* convert TimeObject to the given precision *)
cCustomDateList[TimeObject[{hour_,minute_:0,second_:0},__],type:_String|_Integer]:=Switch[type,
"Hour"|7,hour,
"Minute"|8,{hour,minute},
"Second"|9,{hour,minute,Floor[second]},
"Instant"|10,{hour,minute,second}
]

(* general form *)
cDateWithinQ[date1:_String|_DateObject|{Repeated[_Integer|_Real,{1,6}]},date2:_String|_DateObject|{Repeated[_Integer|_Real,{1,6}]}]:=cDateWhitinComparison[cDateBoundsCore[date1],cDateBoundsCore[date2]]

(* TimeObject comparison *)
(* cDateWithinQ[date1_TimeObject,date2_TimeObject]:=Block[{date1Percision=cDatePrecision[date1]},And[date1Percision<=cDatePrecision[date2],cCustomDateList[date1,date1Percision]==cCustomDateList[date2,date1Percision]]] *)
cDateWithinQ[time1_TimeObject,time2_TimeObject]:=cDateWhitinComparison[cDateBoundsCore[time2],cDateBoundsCore[time2]]

(* DateInterval as first/second input *)
cDateWithinQ[date1_DateInterval,date2:_String|_DateObject|{Repeated[_Integer|_Real,{1,6}]}]:=AnyTrue[date1["Dates"],cDateWhitinComparison[cDateBoundsCore[#],cDateBoundsCore[date2]]&]
cDateWithinQ[date1:_String|_DateObject|{Repeated[_Integer|_Real,{1,6}]},date2_DateInterval]:=AllTrue[date2["Dates"],cDateWhitinComparison[cDateBoundsCore[date1],cDateBoundsCore[#]]&]

(* both arguments are DateInverval *)
cDateWithinQ[date1_DateInterval,date2_DateInterval]:=AllTrue[date2["Dates"],AnyTrue[date1["Dates"],Function[x,cDateWhitinComparison[cDateBoundsCore@x,cDateBoundsCore@#]]]&]

(* make it listable for first/second/both arguments *)
cDateWithinQ[dates1:{Repeated[_String|_DateObject|_TimeObject|_DateInterval|{Repeated[_Integer|_Real,{1,6}]}]},date2:_String|_DateObject|_TimeObject|_DateInterval|{Repeated[_Integer|_Real,{1,6}]}]:=Thread[Unevaluated@cDateWithinQ[dates1,date2],List,{1}]
cDateWithinQ[date1:_String|_DateObject|_TimeObject|_DateInterval|{Repeated[_Integer|_Real,{1,6}]},dates2:{Repeated[_String|_DateObject|_TimeObject|_DateInterval|{Repeated[_Integer|_Real,{1,6}]}]}]:=Thread[Unevaluated@cDateWithinQ[date1,dates2],List,{2}]
cDateWithinQ[date1:{Repeated[_String|_DateObject|_TimeObject|_DateInterval|{Repeated[_Integer|_Real,{1,6}]}]},dates2:{Repeated[_String|_DateObject|_TimeObject|_DateInterval|{Repeated[_Integer|_Real,{1,6}]}]}]:=Thread[Unevaluated@cDateWithinQ[date1,dates2],List]

(* raising Message *)
cDateWithinQ[date1_TimeObject,date2:Except[_TimeObject]]:=(Message[cDateWithinQ::invalidArgument,date2,"comparing with a TimeObject"];False)
cDateWithinQ[date1:Except[_TimeObject],date2_TimeObject]:=(Message[cDateWithinQ::invalidArgument,date1,"comparing with a TimeObject"];False)
cDateWithinQ[date1:Except[_DateObject|_String|_DateInterval|{Repeated[_Integer|_Real,{1,6}]}|{Repeated[_DateObject|_String|_DateInterval|{Repeated[_Integer|_Real,{1,6}]}]}],_]:=(Message[cDateWithinQ::invalidArgument,date1,"first start"];False);
cDateWithinQ[_,date2:Except[_DateObject|_String|_DateInterval|{Repeated[_Integer|_Real,{1,6}]}|{Repeated[_DateObject|_String|_DateInterval|{Repeated[_Integer|_Real,{1,6}]}]}]]:=(Message[cDateWithinQ::invalidArgument,date2,"second start"];False);

End[];



(* ::Subsection:: *)
(*cDateOverlapsQ*)


cDateOverlapsQ::usage="Alternative to Mathematica built-in DateOverlapsQ.";
cDateOverlapsQ::invalidArgument="The argument `1` is not a valid argument for the `2`.";
SyntaxInformation[cDateOverlapsQ]={"ArgumentsPattern"->{_,_}};

Begin["`Private`"];

(* check whether value is between lowerRange and upperRange *)
cDateOverlapComparison[{lowerRange:_Integer|_Real,upperRange:_Integer|_Real},value:_Integer|_Real]:=lowerRange<=value<=upperRange

(* check whether upperValue/lowerValue is between the range of first argument *)
cDateOverlapComparison[{lowerRange:_Integer|_Real,upperRange:_Integer|_Real},{lowerValue:_Integer|_Real,upperValue:_Integer|_Real}]:=lowerRange<lowerValue<upperRange||lowerRange<upperValue<upperRange||And[upperValue>=upperRange,lowerValue<=lowerRange]

(* general form *)
cDateOverlapsQ[date1:_String|_DateObject|{Repeated[_Integer|_Real,{1,6}]},date2:_String|_DateObject|{Repeated[_Integer|_Real,{1,6}]}]:=cDateOverlapComparison[cDateBoundsCore[date1],cDateBoundsCore[date2]]

(* TimeObject comparison *)
cDateOverlapsQ[date1_TimeObject,date2_TimeObject]:=Block[{minPercision=Min[cDatePrecision[date1],cDatePrecision[date2]]},cCustomDateList[date1,minPercision]==cCustomDateList[date2,minPercision]]

(* DateInterval as first/second input *)
cDateOverlapsQ[date1_DateInterval,date2:_String|_DateObject|{Repeated[_Integer|_Real,{1,6}]}]:=AnyTrue[date1["Dates"],cDateOverlapComparison[cDateBoundsCore@date2,cDateBoundsCore@#]&]
cDateOverlapsQ[date1:_String|_DateObject|{Repeated[_Integer|_Real,{1,6}]},date2_DateInterval]:=AnyTrue[date2["Dates"],cDateOverlapComparison[cDateBoundsCore@date1,cDateBoundsCore@#]&]

(* both arguments are DateInverval *)
cDateOverlapsQ[date1_DateInterval,date2_DateInterval]:=AnyTrue[date1["Dates"], AnyTrue[date2["Dates"],Function[x,cDateOverlapComparison[cDateBoundsCore@x,cDateBoundsCore@#]]]&]

(* make it Listable - first/second/both *)
cDateOverlapsQ[dates1:{Repeated[_String|_DateObject|_TimeObject|_DateInterval|{Repeated[_Integer|_Real,{1,6}]}]},date2:_String|_DateObject|_TimeObject|_DateInterval|{Repeated[_Integer|_Real,{1,6}]}]:=Thread[Unevaluated@cDateOverlapsQ[dates1,date2],List,{1}]
cDateOverlapsQ[date1:_String|_DateObject|_TimeObject|_DateInterval|{Repeated[_Integer|_Real,{1,6}]},dates2:{Repeated[_String|_DateObject|_TimeObject|_DateInterval|{Repeated[_Integer|_Real,{1,6}]}]}]:=Thread[Unevaluated@cDateOverlapsQ[date1,dates2],List,{2}]
cDateOverlapsQ[date1:{Repeated[_String|_DateObject|_TimeObject|_DateInterval|{Repeated[_Integer|_Real,{1,6}]}]},dates2:{Repeated[_String|_DateObject|_TimeObject|_DateInterval|{Repeated[_Integer|_Real,{1,6}]}]}]:=Thread[Unevaluated@cDateOverlapsQ[dates1,dates2]]

(* raise Message for invalid arguments *)
cDateOverlapsQ[date1_TimeObject,date2:Except[_TimeObject]]:=(Message[cDateOverlapsQ::invalidArgument,date2,"comparing with a TimeObject"];False)
cDateOverlapsQ[date1:Except[_TimeObject],date2_TimeObject]:=(Message[cDateOverlapsQ::invalidArgument,date1,"comparing with a TimeObject"];False)
cDateOverlapsQ[date1:Except[_DateObject|_String|{Repeated[_Integer|_Real,{1,6}]}|{Repeated[_DateObject|_String|{Repeated[_Integer|_Real,{1,6}]}]}],_]:=(Message[cDateOverlapsQ::invalidArgument,date1,"date1"];False);
cDateOverlapsQ[_,date2:Except[_DateObject|_String|{Repeated[_Integer|_Real,{1,6}]}|{Repeated[_DateObject|_String|{Repeated[_Integer|_Real,{1,6}]}]}]]:=(Message[cDateOverlapsQ::invalidArgument,date2,"date2"];False);

End[];



(* ::Subsection:: *)
(*cNextDate*)


cNextDate::usage="Alternative to Mathematica built-in NextDate.";
cNextDate::invalidArgument="The argument `1` is not a valid argument for the `2`.";
SyntaxInformation[cNextDate]={"ArgumentsPattern"->{_,_.}};

Begin["`Private`"];
ClearAll[granularityPrecision];
granularityPrecision::invalidArgument="Input `1` is not a valid granularity.";
granularityPrecision[type:_String|_Symbol]:=Switch[type,
"Millennium",1,
"MillenniumBeginning01",1.1,
"Century",2,
"CenturyBeginning01",2.1,
"Decade",3,
"Year",4,
"Quarter",4.5,
"Month",5,
"Week",5.5,
"WeekBeginningSunday",5.75,
"Day"|"Weekday"|"WeekendDay"|"Weekend"|"BeginningOfMonth"|"EndOfMonth"|"MonthFirstDay"|"MonthLastDay"|"BusinessDay"|Sunday|Monday|Tuesday|Wednesday|Thursday|Friday|Saturday,6,
"Hour",7,
"Minute",8,
"Second",9,
"Instant",10,
_,Message[granularityPrecision::invalidArgument,type]
]

(* date argument is not provided *)
cNextDate[type:_String|_Symbol]:=Check[cNextDateCore[AbsoluteTime[],type],Defer@cNextDate[type]]

(* for TimeObject *)
cNextDate[date_TimeObject,type:"Millisecond"|"Second"|"Minute"|"Hour"]:=Switch[type,
"Millisecond",TimeObject[Take[DateList[date]+{0,0,0,0,0,10^-9},{4,6}],"Instant"],
"Second",TimeObject[Take[DateList[date]+{0,0,0,0,0,1},{4,6}],"Second"],
"Minute",TimeObject[Take[DateList[date]+{0,0,0,0,1,0},{4,5}],"Minute"],
"Hour",TimeObject[Take[DateList[date]+{0,0,0,1,0,0},{4}],"Hour"]
]

(* date argument needs conversion *)
cNextDate[date:_DateObject|_String|{Repeated[_Integer|_Real,{1,6}]},type:_String|_Symbol]:=Check[
If[cDatePrecision[date]<granularityPrecision[type],
cNextDateCore[Echo[Last[cDateBoundsCore[date]]-(10.^-9)],type],
cNextDateCore[AbsoluteTime[date],type]
],Defer@cNextDate[date,type]]

(* date argument is a number *)
cNextDate[date:_Integer|_Real,type:_String|_Symbol]:=cNextDateCore[date,type]

(* do the calculations *)
cNextDateCore[date:_Integer|_Real,type:_String|_Symbol]:=Switch[type,

"Second",DateObject[MapAt[Floor,DateList[date],6]+{0,0,0,0,0,1},type],
"Minute",DateObject[Most@DateList[date]+{0,0,0,0,1},type],
"Hour",DateObject[Take[DateList[date],4]+{0,0,0,1},type],
"Day",DateObject[Take[DateList[date],3]+{0,0,1},type],

Monday,DateObject[Take[DateList[date],3]+{0,0,{7,6,5,4,3,2,1}[[cDayNameIndex[date]]]},"Day"],
Tuesday,DateObject[Take[DateList[date],3]+{0,0,{1,7,6,5,4,3,2}[[cDayNameIndex[date]]]},"Day"],
Wednesday,DateObject[Take[DateList[date],3]+{0,0,{2,1,7,6,5,4,3}[[cDayNameIndex[date]]]},"Day"],
Thursday,DateObject[Take[DateList[date],3]+{0,0,{3,2,1,7,6,5,4}[[cDayNameIndex[date]]]},"Day"],
Friday,DateObject[Take[DateList[date],3]+{0,0,{4,3,2,1,7,6,5}[[cDayNameIndex[date]]]},"Day"],
Saturday,DateObject[Take[DateList[date],3]+{0,0,{5,4,3,2,1,7,6}[[cDayNameIndex[date]]]},"Day"],
Sunday,DateObject[Take[DateList[date],3]+{0,0,{6,5,4,3,2,1,7}[[cDayNameIndex[date]]]},"Day"],

"BusinessDay",Block[{temp=date+{1,1,1,1,3,2,1}[[cDayNameIndex[date]]]*86400.},If[cHolidayQ[temp],cNextDate[temp,"BusinessDay"],DateObject[temp,"Day"]]],

"Weekday",DateObject[Take[DateList[date],3]+{0,0,{1,1,1,1,3,2,1}[[cDayNameIndex[date]]]},"Day"],
"Weekend"|"WeekendDay",DateObject[Take[DateList[date],3]+{0,0,{5,4,3,2,1,1,6}[[cDayNameIndex[date]]]},"Day"],

"Week",DateObject[Take[DateList[date],3]+{0,0,{7,6,5,4,3,2,1}[[cDayNameIndex[date]]]},type],
"WeekBeginningSunday",DateObject[Take[DateList@date,3]+{0,0,{6,5,4,3,2,1,7}[[cDayNameIndex[date]]]},type],

"Month",DateObject[{#1,#2+1}&@@DateList[date],type],
"BeginningOfMonth"|"MonthFirstDay",DateObject[Take[DateList[date],3]*{1,1,0}+{0,1,1},"Day"],
"EndOfMonth"|"MonthLastDay",DateObject[Take[DateList[Floor[date,86400.]+86400.],3]*{1,1,0}+{0,1,0},"Day"],

"Quarter",DateObject[Apply[({#1,0}+Switch[#2,1|2|3,{0,4},4|5|6,{0,7},7|8|9,{0,10},10|11|12,{If[#1==-1,2,1],1}])&,Take[DateList[date],2]],type],

"Year",DateObject[{If[#==-1,0,#]&[First@DateList[date]]+1},type],
"Decade",DateObject[{If[#==0,#+1,#]&@Ceiling[First@DateList[date]+1,10]},type],

"Century",Block[{roundedYear=Ceiling[First@DateList[date]+1,100]},DateObject[{If[roundedYear==0,roundedYear+1,roundedYear]},type]],
"CenturyBeginning01",DateObject[{Ceiling[First@DateList[date],100]+1},type],

"Millennium",Block[{roundedYear=Ceiling[First@DateList@date+1,1000]},DateObject[{If[roundedYear==0,roundedYear+1,roundedYear]},type]],
"MillenniumBeginning01",DateObject[{Ceiling[First@DateList[date],1000]+1},type],
"Holiday",DateObject[cNextHoliday[date],"Day"],
_,Message[cNextDate::invalidArgument,type,"type"];Defer[cNextDate[date,type]]]

(* raise Message for invalid arguments *)
cNextDate[_TimeObject,type:Except["Millisecond"|"Second"|"Minute"|"Hour"]]/;Message[cNextDate::invalidArgument,type,"type for TimeObject"]=False;
cNextDate[date:Except[_Integer|_Real|_TimeObject|_DateObject|_String|{Repeated[_Integer|_Real,{1,6}]}],_]/;Message[cNextDate::invalidArgument,date,"date"]=False;
cNextDate[_:Null,type:Except[_String|_Symbol]]/;Message[cNextDate::invalidArgument,type,"type"]=False;

End[];



(* ::Subsection:: *)
(*cPreviousDate*)


cPreviousDate::usage="Alternative to Mathematica built-in PreviousDate.";
cPreviousDate::invalidArgument="The argument `1` is not a valid argument for the `2`.";
SyntaxInformation[cPreviousDate]={"ArgumentsPattern"->{_,_.}};

Begin["`Private`"];

(* date argument is not provided *)
cPreviousDate[type:_String|_Symbol]:=Check[cPreviousDateCore[AbsoluteTime[],type],Defer@cPreviousDate[type]]

(* for TimeObject *)
cPreviousDate[start_TimeObject,type:"Millisecond"|"Second"|"Minute"|"Hour"]:=Switch[type,
"Millisecond",TimeObject[Take[DateList[date]-{0,0,0,0,0,10^-9},{4,6}],"Instant"],
"Second",TimeObject[Take[DateList[date]-{0,0,0,0,0,1},{4,6}],"Second"],
"Minute",TimeObject[Take[DateList[date]-{0,0,0,0,1,0},{4,5}],"Minute"],
"Hour",TimeObject[Take[DateList[date]-{0,0,0,1,0,0},{4}],"Hour"]
]

(* date argument needs conversion *)
cPreviousDate[date:_DateObject|_String|{Repeated[_Integer|_Real,{1,6}]},type:_String|_Symbol]:=Check[cPreviousDateCore[AbsoluteTime[date],type],Defer@cPreviousDate[date,type]]

(* date argument is a number *)
cPreviousDate[date:_Integer|_Real,type:_String|_Symbol]:=cPreviousDateCore[date,type]

(* do the calculations *)
cPreviousDateCore[date:_Integer|_Real,type:_String|_Symbol]:=Switch[type,
"Millisecond",DateObject[DateList[date]-{0,0,0,0,0,1},type],
"Second",DateObject[MapAt[Floor,DateList[date],6]-{0,0,0,0,0,1},type],
"Minute",DateObject[Most@DateList@date-{0,0,0,0,1},type],
"Hour",DateObject[Take[DateList@date,4]-{0,0,0,1},type],
"Day",DateObject[Take[DateList@date,3]-{0,0,1},type],

Monday,DateObject[Take[DateList[date],3]+{0,0,{-7,-1,-2,-3,-4,-5,-6}[[cDayNameIndex[date]]]},"Day"],
Tuesday,DateObject[Take[DateList[date],3]+{0,0,{-6,-7,-1,-2,-3,-4,-5}[[cDayNameIndex[date]]]},"Day"],
Wednesday,DateObject[Take[DateList[date],3]+{0,0,{-5,-6,-7,-1,-2,-3,-4}[[cDayNameIndex[date]]]},"Day"],
Thursday,DateObject[Take[DateList[date],3]+{0,0,{-4,-5,-6,-7,-1,-2,-3}[[cDayNameIndex[date]]]},"Day"],
Friday,DateObject[Take[DateList[date],3]+{0,0,{-3,-4,-5,-6,-7,-1,-2}[[cDayNameIndex[date]]]},"Day"],
Saturday,DateObject[Take[DateList[date],3]+{0,0,{-2,-3,-4,-5,-6,-7,-1}[[cDayNameIndex[date]]]},"Day"],
Sunday,DateObject[Take[DateList[date],3]+{0,0,{-1,-2,-3,-4,-5,-6,-7}[[cDayNameIndex[date]]]},"Day"],

"BusinessDay",Block[{temp=date+{-3,-1,-1,-1,-1,-1,-2}[[cDayNameIndex[date]]]*86400.},If[cHolidayQ[temp],Return@cPreviousDate[temp,"BusinessDay"],DateObject[temp,"Day"]]],

"Weekday",DateObject[Take[DateList[date],3]+{0,0,{-3,-1,-1,-1,-1,-1,-2}[[cDayNameIndex@date]]},"Day"],
"Weekend"|"WeekendDay",DateObject[Take[DateList[date],3]+{0,0,{-1,-2,-3,-4,-5,-6,-1}[[cDayNameIndex@date]]},"Day"],

"Week",DateObject[Take[DateList[date],3]+{0,0,{-7,-8,-9,-10,-11,-12,-13}[[cDayNameIndex[date]]]},type],
"WeekBeginningSunday",DateObject[Take[DateList[date],3]+{0,0,{-8,-9,-10,-11,-12,-13,-7}[[cDayNameIndex[date]]]},type],

"Month",DateObject[{#1,#2-1}&@@DateList[date],type],
"EndOfMonth"|"MonthLastDay",DateObject[Take[DateList[date],3]*{1,1,0},"Day"],
"BeginningOfMonth"|"MonthFirstDay",DateObject[Take[DateList[date-86400.],3]*{1,1,0}+{0,0,1},"Day"],

"Quarter",DateObject[Apply[({#1,0}+Switch[#2,1|2|3,{-1,10},4|5|6,{0,1},7|8|9,{0,4},10|11|12,{0,7}])&,Take[DateList[date],2]],type],
"Year",DateObject[{First@DateList[date]-1},type],
"Decade",DateObject[{If[#==0,1,#]&@Floor[First@DateList[date]-10,10]},type],
"Century",DateObject[{If[#==0,1,#]&@Floor[First@DateList[date]-100,100]},type],
"CenturyBeginning01",DateObject[{Floor[First@DateList[date]-101,100]+1},type],
"Millennium",DateObject[{If[#==0,1,#]&@Floor[First@DateList[date]-1000,1000]},type],
"MillenniumBeginning01",DateObject[{Floor[First@DateList[date]-1001,1000]+1},type],
"Holiday",DateObject[cPreviousHoliday[date],"Day"],
_,Message[cPreviousDate::invalidArgument,type,"type"];Defer[cPreviousDate[date,type]]
]

(* raise Message for invalid arguments *)
cPreviousDate[_TimeObject,type:Except["Millisecond"|"Second"|"Minute"|"Hour"]]/;Message[cPreviousDate::invalidArgument,type,"type for TimeObject"]=False;
cPreviousDate[date:Except[_Integer|_Real|_TimeObject|_DateObject|_String|{Repeated[_Integer|_Real,{1,6}]}],_]/;Message[cPreviousDate::invalidArgument,date,"date"]=False;
cPreviousDate[_:Null,type:Except[_String|_Symbol]]/;Message[cPreviousDate::invalidArgument,type,"type"]=False;

End[];



(* ::Subsection:: *)
(*cCurrentDate*)


cCurrentDate::usage="Alternative to Mathematica built-in CurrentDate.";
cCurrentDate::invalidArgument="The argument `1` is not a valid argument for the `2`.";
SyntaxInformation[cCurrentDate]={"ArgumentsPattern"->{_,_.}};

Begin["`Private`"];

(* date argument is not provided *)
cCurrentDate[type:_String|_Symbol]:=Check[cPreviousDate[AbsoluteTime[],type],Defer@cCurrentDate[type]]

(* for TimeObject *)
cCurrentDate[date_TimeObject,type:"Millisecond"|"Second"|"Minute"|"Hour"]:=Switch[type,
"Millisecond",TimeObject[Take[DateList[date],{4,6}],"Instant"],
"Second",TimeObject[Take[DateList[date],{4,6}],"Second"],
"Minute",TimeObject[Take[DateList[date],{4,5}],"Minute"],
"Hour",TimeObject[Take[DateList[date],{4}],"Hour"]
]

(* date argument needs argument *)
cCurrentDate[date:_Integer|_Real|_String|_DateObject|{Repeated[_Integer|_Real,{1,6}]},type:_String|_Symbol]:=Switch[type,
"Second"|"Minute"|"Hour"|"Day"|"Week"|"WeekBeginningSunday"|"Month"|"Quarter"|"Year"|"Decade"|"Century"|"CenturyBeginning01"|"Millennium"|"MillenniumBeginning01",DateObject[DateList@date,type],

Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday|"BusinessDay"|"Weekday"|"Weekend"|"WeekendDay"|"BeginningOfMonth"|"MonthFirstDay"|"EndOfMonth"|"MonthLastDay",If[cDayMatchQ[date,type],DateObject[date,"Day"],Message[cCurrentDate::invalidArgument,date,"date"];Defer@cCurrentDate[date,type]],

_,Message[cCurrentDate::invalidArgument,type,"type"];Defer[cCurrentDate[date,type]]
]

(* raise Message for invalid arguments *)
cCurrentDate[_TimeObject,type:Except["Millisecond"|"Second"|"Minute"|"Hour"]]/;Message[cCurrentDate::invalidArgument,type,"type for TimeObject"]=False;
cCurrentDate[date:Except[_Integer|_Real|_TimeObject|_DateObject|_String|{Repeated[_Integer|_Real,{1,6}]}],_]/;Message[cCurrentDate::invalidArgument,date,"date"]=False;
cCurrentDate[_:Null,type:Except[_String|_Symbol]]/;Message[cPreviousDate::invalidArgument,type,"type"]=False;

End[];



(* ::Subsection:: *)
(*cDateBounds*)


cDateBounds::usage="Alternative to Mathematica built-in DateBounds.";
cDateBounds::invalidArgument="The argument `1` is not a valid argument for the `2`.";
SyntaxInformation[cDateBounds]={"ArgumentsPattern"->{_,_.}};

Begin["`Private`"];

ClearAll[cDatePrecisionClipped];

cDatePrecisionClipped[start:_String|_DateObject|_TimeObject|{Repeated[_Integer|_Real,{1,6}]}]:=Switch[start,
_DateObject|_List|_String,10,
_TimeObject,Switch[start[[2]],"Hour",7,"Minute",8,"Second",9,"Instant",10]
]

cNumberDecomposeForTimeObject[number_]:=Drop[Catenate@Rest@FoldList[QuotientRemainder[Last@#1, #2] &, {number}, {3600, 60}], {2}]

(* base on a DateList format and type, return the related bound *)
(* used interally as well for other functions *)
cDateBoundsCore[{year_,month_:1,day_:1,hour_:0,minute_:0,second_:0},type:_Integer|_Real]:=Switch[type,
(* Millennium *)            1,Block[{lowerYear=If[#==0,1,#]&@Floor[If[year==0,-1,year],1000]},{AbsoluteTime[{lowerYear}],AbsoluteTime[{lowerYear+Switch[lowerYear,-1|-1000,1001,_,1000]}]}],
(* MillenniumBeginning01 *) 1.1,Block[{lowerYear=Floor[If[year==0,-1,year],1000]},{AbsoluteTime[{lowerYear+1}],AbsoluteTime[{If[lowerYear==-1000,2,lowerYear+1001]}]}],
(* Century *)               2,Block[{lowerYear=If[#==0,1,#]&@Floor[If[year==0,-1,year],100]},{AbsoluteTime[{lowerYear}],AbsoluteTime[{If[lowerYear==-100,1,lowerYear+100]}]}],
(* CenturyBeginning01 *)    2.1,Block[{lowerYear=Floor[If[year==0,-1,year],100]},{AbsoluteTime[{lowerYear+1}],AbsoluteTime[{If[lowerYear==-100,2,lowerYear+101]}]}],
(* Decade *)                3,Block[{lowerYear=If[#==0,1,#]&@Floor[If[year==0,-1,year],10]},{AbsoluteTime[{lowerYear}],AbsoluteTime[{lowerYear+If[lowerYear==-10,11,10]}]}],
(* Year *)                  4,{AbsoluteTime[{year}],AbsoluteTime[{If[year==-1,1,year+1]}]},
(* Quarter *)               4.5,Block[{quarter=Quotient[DateList[{year,month,day}][[2]],3,1]+1},{AbsoluteTime[{year,(quarter-1)*3+1}],AbsoluteTime[{If[year==-1&&quarter==4,0,year],(quarter-1)*3+4}]}],
(* Month *)                 5,{AbsoluteTime[{year,month}],AbsoluteTime[{If[year==-1&&month==12,0,year],month+1}]},
(* Week *)                  5.5,AbsoluteTime[{year,month,day-cDayRoundBackwardValue[AbsoluteTime@{year,month,day},Monday]*86400}]+{0,604800},
(* WeekBeginningSunday *)   5.75,AbsoluteTime[{year,month,day-#*86400}]&@cDayRoundBackwardValue[AbsoluteTime@{year,month,day},Sunday]+{0,604800},
(* Day *)                   6,AbsoluteTime[{year,month,day}]+{0,86400},
(* Hour *)                  7,AbsoluteTime[{year,month,day,hour}]+{0,3600},
(* Minute *)                8,AbsoluteTime[{year,month,day,hour,minute}]+{0,60},
(* Second *)                9,AbsoluteTime[{year,month,day,hour,minute,Floor@second}]+{0,1},
(* Instant *)               10,AbsoluteTime[{year,month,day,hour,minute,Floor[second,10.^-9]}]+{0,10.^-9}
](* assume instant has nanosecond precision *)

cDateBoundsCore[date_DateObject]:=cDateBoundsCore[First@date,cDatePrecision[date]]
cDateBoundsCore[date:_String|{Repeated[_Integer|_Real,{1,6}]}]:=cDateBoundsCore[DateList@date,cDatePrecision[date]]
cDateBoundsCore[date:{Repeated[_String|_DateObject|{Repeated[_Integer|_Real,{1,6}]}]}]:={AbsoluteTime[#1],Last@cDateBounds[#2]}&@@SortBy[date,AbsoluteTime][[{1,-1}]]

(* for TimeObject *)
cDateBoundsCore[TimeObject[{hour_,minute_:0,second_:0},type_,___]]:=Switch[type,
"Hour",hour*3600+{0,3600},
"Minute",(hour*3600+minute*60)+{0,60},
"Second"|_/;IntegerQ[second],(hour*3600+minute*60+Floor@second)+{0,1},
"Instant",(hour*3600+minute*60+Floor[second,10^-9])+{0,10.^-9}
]

(* find date bound - specific for DateObject *)
cDateBounds[date_DateObject]:=DateObject[#,"Instant"]&/@cDateBoundsCore[First@date,cDatePrecision[date]]

(* for other type *)
cDateBounds[date:_String|{Repeated[_Integer|_Real,{1,6}]}]:=Check[If[Head[date]==List,DateListExtended,Identity]/@cDateBoundsCore[DateList@date,cDatePrecision[date]],Defer[cDateBounds[date]]]

cDateBounds[date_TimeObject]:=TimeObject[cNumberDecomposeForTimeObject@#,"Instant"]&/@cDateBoundsCore[date]

cDateBounds[date_TemporalData]:={date["FirstDate"],date["LastDate"]}
cDateBounds[date_Databin]:=cDateBounds@date["Timestamps"]
cDateBounds[date_DateInterval]:=date["DateBounds"]

(* for a list of date in various formats *)
cDateBounds[date:{Repeated[_String|_DateObject|{Repeated[_Integer|_Real,{1,6}]}]}]:=Check[SortBy[date,AbsoluteTime][[{1,-1}]],Defer[cDateBounds[date]]]

cDateBounds[date:_DateObject|_TimeObject|_String|{Repeated[_Integer|_Real,{1,6}]}|_TemporalData|_Databin|_DateInterval,type_String]:=Check[If[!MatchQ[#,_Defer],Map[DateObject[#,type]&],#]&@cDateBounds[date],Message[cDateBounds::invalidArgument,type,"endpoints type"];Defer[cDateBounds[date,type]]]

(* raise error for invalid arguments *)
cDateBounds[date:_DateObject|_TimeObject|_String|{Repeated[_Integer|_Real,{1,6}]}|_TemporalData|_Databin|_DateInterval]/;Message[cDateBounds::invalidArgument,date,"date"]=False;
cDateBounds[date:_DateObject|_TimeObject|_String|{Repeated[_Integer|_Real,{1,6}]}|_TemporalData|_Databin|_DateInterval,_]:=(Message[cDateBounds::invalidArgument,date,"date"];Defer[cDateBounds[date,type]])
cDateBounds[_,type_]:=(Message[cDateBounds::invalidArgument,type,"endpoints type"];Defer[cDateBounds[date,type]])

End[];



(* ::Subsection:: *)
(*cCalendarView*)


(* need some re-archituring *)

Begin["`Private`"];

(* cDateGreater[date1_DateObject,date2_DateObject]:=VectorGreater[{cDateBoundsCore[date1],Last@cDateBoundsCore[date2]}]
cDateGreaterEqual[date1_DateObject,date2_DateObject]:=VectorGreaterEqual[{cDateBoundsCore[date1],Last@cDateBoundsCore[date2]}]
cDateLess[date1_DateObject,date2_DateObject]:=VectorLess[{cDateBoundsCore[date1],Last@cDateBoundsCore[date2]}]
cDateLessEqual[date1_DateObject,date2_DateObject]:=VectorLessEqual[{cDateBoundsCore[date1],Last@cDateBoundsCore[date2]}] *)

cMapAt[fn_,data_,pos_Span]/;FreeQ[pos,List]:=MapAt[fn,data,pos];
cMapAt[fn_,data_,pos_Span]:=Fold[MapAt[fn,#1,#2]&,data,Replace[Thread@pos,x_Span:>{x}]];
cMapAt[fn_,data_,pos_?ArrayQ]:=MapAt[fn,data,List/@Select[pos,0<#<=Length@data&]];

allowedHighlightsTypes=_Span|_String|_Symbol|_Integer|_Real|_DateObject|_Rule|{(_Integer|_Real|_Rule)...}|{Repeated[_Rule|_DateObject|{Repeated[_Integer|_Real,{3,6}]}]};

cCircleAround[item_,color_:Pink]:=Graphics[{Style[Circle[],color,Thickness[.1]],Inset[Text[item]]},ImageSize->{20,20}];

cMapAtHighlight[{start:_Integer|_Real,end:_Integer|_Real},data_?ArrayQ,pos_?(FreeQ[Rule]),color:_?ColorQ:Pink]:=cMapAt[If[MatchQ[#,_Highlighted|_Integer],cCircleAround[#,color],#]&,data,cHighlightPositionConverter[{start,end},pos,Count[Take[data,7],""]]]

cMapAtHighlight[{start:_Integer|_Real,end:_Integer|_Real},data_?ArrayQ,pos_?(FreeQ[Rule]),possibleGrayIndicies:{(_Integer|{_Integer})...},mainColor:_?ColorQ:Pink,outOfRangeColor:_?ColorQ:GrayLevel[.6]]:=MapAt[If[MatchQ[#,_Graphics],ReplacePart[#,{1,1,2}->outOfRangeColor],#]&,cMapAtHighlight[{start,end},data,pos,mainColor],possibleGrayIndicies]

cMapAtHighlight[{start:_Integer|_Real,end:_Integer|_Real},data_?ArrayQ,Rule[pos_,color_?ColorQ],_:{(_Integer|{_Integer})...}]:=cMapAtHighlight[{start,end},data,pos,color]

(* postion can be list of rules or rule with plain positions *)
cMapAtHighlight[{start:_Integer|_Real,end:_Integer|_Real},data_?ArrayQ,coloredPos_,_:{(_Integer|{_Integer})...}]:=Fold[cMapAtHighlight[{start,end},#1,First@#2,Last@#2]&,data,Replace[coloredPos,x:Except[_Rule]:>Rule[x,Pink],{1}]]

cHighlightPositionConverter[{start:_Integer|_Real,end:_Integer|_Real},pos_,offset_]:=Switch[pos,
"Holiday"|"BusinessDay",cHighlightPositionConverter[{start,end},FromAbsoluteTime/@cDayRangeValue[start,end,pos],offset],
_Symbol|_String|{Repeated[_Symbol|_String]},ReplaceAll[pos,{Monday->1,Tuesday->2,Wednesday->3,Thursday->4,Friday->5,Saturday->6,Sunday->7,"Weekend"->{6,7},"Weekday"->{1,2,3,4,5},All|"Day"->{1,2,3,4,5,6,7}}];;;;7,
_Integer|_Real|_DateObject|{Repeated[_Integer|_Real,{3,6}]},{offset+Quotient[Floor[AbsoluteTime@pos,86400]-Floor[start,86400],86400]+1},
{Repeated[_Integer|_Real|_DateObject|{Repeated[_Integer|_Real,{3,6}]}]},offset+Quotient[Floor[AbsoluteTime/@pos,86400]-Floor[start,86400],86400]+1,
_Rule,MapAt[cHighlightPositionConverter[{start,end},#,offset]&,pos,{1}],
{__Rule},MapAt[cHighlightPositionConverter[{start,end},#,offset]&,pos,{All,1}],
_,pos
]


(* generate padded array of numbers *)
cCalendarCore[numberofDays_Integer,offset_Integer]:=ArrayPad[Range[numberofDays],{offset-1,0},""]

(* , FrameMargins -> 0 *)

(* generate padded array of numbers for multiple month *)
cCalendarCore[numberofDays:{__Integer},offset_Integer]:=ArrayPad[Catenate@Table[MapAt[Highlighted[#,RoundingRadius->50,ImageSize->{15,15},BaseStyle->10] &, Range[i], 1], {i, numberofDays}], {offset - 1, 0}, ""]

(* view the matrix as calendar format with  *)

cCalendarViewCore[numbersMatrix_?ArrayQ,title_String:"Calendar"]:=
 Labeled[TableForm[Partition[numbersMatrix,7,7,1,""], 
   TableHeadings -> {None, {"M", "Tu", "W", "Th", "F", "Sa", "Su"}}, 
   TableSpacing -> {2, 2}, TableAlignments -> {Center, Center}], title, Top,LabelStyle->11]

cCalendarView[date:_Integer|_Real|_DateObject|{Repeated[_Integer|_Real,{1,6}]},highlights:allowedHighlightsTypes:{}]:= Block[{temp=Take[DateList[date], 2]},
 cCalendarViewCore[cMapAtHighlight[cDateBoundsCore[temp],cCalendarCore[cMonthListWithDay@@temp, 
  cDayNameIndex[AbsoluteTime@Take[DateList@date,2]]],highlights],DateString[DateObject@date, {"Year", "-", "Month", " (", "MonthName", ")"}]]
]

cCalendarMultipleView[date:_Integer|_Real|_DateObject|{Repeated[_Integer|_Real,{1, 6}]},monthRangeEnd_Integer?Positive,highlights:allowedHighlightsTypes:{}] := cCalendarMultipleView[date,{0,monthRangeEnd},highlights]

cCalendarMultipleView[date:_Integer|_Real|_DateObject|{Repeated[_Integer|_Real,{1, 6}]},monthRangeStart_Integer?Negative,highlights:allowedHighlightsTypes:{}] := cCalendarMultipleView[date,{monthRangeStart,0},highlights]

cCalendarMultipleView[date:_Integer|_Real|_DateObject|{Repeated[_Integer|_Real,{1, 6}]},{monthRangeStart_,monthRangeEnd_},highlights:allowedHighlightsTypes:{}] := 
Block[{dates = Table[Take[DateList[{#1,#2+i}],2]&@@DateList[date],{i,monthRangeStart,monthRangeEnd}]},
  cCalendarViewCore[cMapAtHighlight[{First@cDateBoundsCore@First@dates,Last@cDateBoundsCore@Last@dates},cCalendarCore[cMonthListWithDay @@@ dates, 
   cDayNameIndex@AbsoluteTime@First@dates], highlights],DateString[First@dates, {"Year", "-", "Month", " (", "MonthNameShort", ")"}]<>" -> "<>DateString[Last@dates, {"Year", "-", "Month", " (", "MonthNameShort", ")"}]]
]

cCalendarRangeView[date1:_Integer|_Real|_DateObject|{Repeated[_Integer|_Real,{1, 6}]},date2:_Integer|_Real|_DateObject|{Repeated[_Integer|_Real,{1, 6}]},highlights:allowedHighlightsTypes:{}]/;AbsoluteTime[date1]>AbsoluteTime[date2]:=cCalendarRangeView[date2,date1,highlights];

cCalendarRangeView[date1:_Integer|_Real|_DateObject|{Repeated[_Integer|_Real,{1, 6}]},date2:_Integer|_Real|_DateObject|{Repeated[_Integer|_Real,{1, 6}]},highlights:allowedHighlightsTypes:{}]:=Block[
{finder,result,start,end,dates,text,offset,startIndex,endIndex,roundedStart,roundedEnd,roundedStartIndex,roundedEndIndex,startEndRoundedRange,possibleGrayRelativeIndicies,dateListDate1=DateList[date1],dateListDate2=DateList[date2]},

finder[data_,item_]:=First@FirstPosition[data,item|Highlighted[item,___],{1}];

dates = Table[Take[DateList[{#1,#2+i}],2]&@@dateListDate1,{i,0,Dot[Take[dateListDate2,2],{12,1}]-Dot[Take[dateListDate1,2],{12,1}]}];

result=cCalendarCore[cMonthListWithDay @@@ dates, cDayNameIndex@AbsoluteTime@First@dates];

offset=Count[Take[result,7],""];

startIndex=finder[result,dateListDate1[[3]]];
endIndex=Length@result-finder[Reverse@result,dateListDate2[[3]]]+1;

roundedStartIndex=(Quotient[startIndex,7,1]*7)+1;
roundedEndIndex=((Quotient[endIndex,7,1]+1)*7);

roundedStart=Ramp[roundedStartIndex-offset];
roundedEnd=Ramp[roundedEndIndex-offset];

possibleGrayRelativeIndicies=If[highlights=!={},List/@Join[Range[roundedStartIndex,startIndex-1],Range[endIndex+1,roundedEndIndex]]-(roundedStartIndex-1),{}];

startEndRoundedRange={AbsoluteTime@{Splice@First@dates,roundedStart},AbsoluteTime@{Splice@Last@dates,roundedEnd}};

result=Take[result,{roundedStartIndex,Clip[roundedEndIndex,{1,Length@result}]}];

text=DateString[DateObject@date1, {"Year", "-", "Month","-","Day", " (", "MonthNameShort", ")"}]<>" -> "<>DateString[DateObject@date2, {"Year", "-", "Month","-","Day", " (", "MonthNameShort", ")"}];

cCalendarViewCore[cMapAtHighlight[startEndRoundedRange,result,highlights,possibleGrayRelativeIndicies],text] 
]


End[];
EndPackage[]

