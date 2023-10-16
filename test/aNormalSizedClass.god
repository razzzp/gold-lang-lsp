class aNormalSizedClass (aFullObject)

type tNumber : Int1 ;(integer type with one byte)
type tHighInMeters : Int2 ;(integer type with two bytes)
type tLetter : char ;(character type)
type tIsOk : boolean ;(boolean type)
type tDiameter : Num4 ;(numerical type with four bytes)
type tMoneyAmount : Decimal ;(decimal type)
type tMarkOfCar : CString31 ;(cstring type with 31 characters)
type tNameOfCar : String31 ;(string type with 31 characters)
type tOpenedHours : 8 to 18
type tNumberOfDay : 1 to 31 ;(Subrange type)
type tUpperCase : 'A' to 'Z' ;(Subrange type)
type tWorkDays : (Monday, Tuesday, Wednesday, Thursday, Friday) ;(enumerated type)
type tDays : tWorkDays + (Saturday, Sunday) ;(enumerated type that complements the previous one)
type tComment: Text

type tSetOfWorkDays : [tWorkDays] ; (Set type)
type tHouse : record ; (Record type)
   NumberOfStairs : Int4
   Area : Int4
   Color : tColors
   Garden : Boolean
endRecord
type tDayComments : array [tDays] of String31
  ;( declaration of a string array in which the <index-declaration> is the enumerated type tDays)	
type tWorkLoad : array [tWorkDays][tOpenedHours] of Int4 
  ;( declaration of an integer two dimensional array)
type tSeqOfI : sequence [Unbounded] of Int4
type tSeq0to10: sequence [0 to 10] of Int4
type tSeqOfText: sequence [Unbounded] of Text
type tSeqOfSeqOfI: sequence [Unbounded] of tSeqOfI
type tSeqOfCString: sequence [Unbounded] of Cstring31
type tSeqOfRecHouse: sequence [Unbounded] of tHouse 
type tSeqOfObject: sequence [Unbounded] of aNamedObject

type tpString : .string ;(pointer type on a string)
type tProc : procedure (I4 : Int4, inOut theMessage : CString) ;(pointer to proc with 2 parameters)
type tRefVehicle: refTo aVehicle ;(refto type referencing the class aVehicle)
type tListOfVehicles: listOf aVehicle ;(listof type referencing the class aVehicle)

type tInstanceType: instanceOf aInstance

MyVariables : refTo [P,A] aType
SecondVar : Int4
MyVariables : listOf [O] aAnotherType
SecondVar : Cstring
MyVariables : .pObject
SecondVar : Int4
MyVariables : refTo [P,A] aType
SecondVar : Int4

proc TestOQL
    forEach curPerson in OQL
    select * from x in aPerson++ where x.LastName like 'S'
        WriteLn(curPerson)
    endFor

    theCursor = Motor.OpenOQLCursor
    OQL select OQLMax(thePers.Salary), OQLMin(thePers.Salary), OQLSum(thePers.Salary),
        OQLCount(*) from thePers in aPerson where thePers.Division = ForDivision using theCursor
    Error = Motor.OQLError
    if Error = 0
        OQL Fetch into MaxPay, MinPay, TotalPay, NumEmployees using theCursor
    else
        Alert('OQL Select error ' + IaS(Error))
    endIf
    Motor.CloseOQLCursor(theCursor)

    forEach curCompany in OQL select * from x in aCompany where (x.Name like self.Name) and
        OQLConditionalWhere(self.City <> '', x.City = self.City) and
        OQLConditionalWhere(self.SearchByManpower, (x.ManPower >= self.MinNumEmployees) and (x.ManPower <= self.MaxNumEmployees)) and
        OQLConditionalWhere(self.SearchByValue, (x.CompanyValue >= self.MinValue) and (x.CompanyValue <= self.MaxValue))
        ; do something
    endFor

    forEach curCompany in OQL select * from x in aCompany where x.City like self.City order by x.CompanyValue descending, x.ManPower
        ; do something
    endFor

    forEach curInfo in OQL select x.LastName, y.LastName from x in aPerson, y in aTeacher where y.myStudents.ContainsObject(x)
        WriteLn(curInfo.x.LastName, ' is in class taught by ', curInfo.y.LastName)
    endFor

    if not Motor.IsNew(self)
      forEach curSmallId in OQL Select top 1 x.NSId, x.Id, x.Version from x in aEntity++
         where (x.Somethin.NSId = self.NSId) and (x.Something.Id = 
         self.Id)
         _Result = True
      endFor
   endIf
endproc


function anotherFunction return Int4
    
endfunc
    
function testFunction return Int4
    ; some function
    return 6
endFunc

procedure testProcedure(firstParam: aType) protected override
    var localVar : aType
    var var2 : int4
    ; some proc
    tetst(asd);
    if (asd + a + var2)
        writeln(something)
    else
        asd = Something - another thing
    endif
    loop
        break
        continue
    endLoop
    exit
    ; some comment sss
endProc

procedure testProcedure1(firstParam: aType) protected override
    ; some proc
    ; some comment sss
endProc

procedure testProcedure2(firstParam: aType) protected override
    ; some proc
    ; some comment sss
endProc

procedure testProcedure3(firstParam: aType) protected override
    ; some proc
    ; some comment sss
endProc



function anotherFunction return Int4
    
endfunc
    
function testFunction return Int4
    ; some function
    return 6
endFunc

procedure testProcedure(firstParam: aType) protected override
    var localVar : aType
    var var2 : int4
    ; some proc
    tetst(asd);
    if (asd + a + var2)
        writeln(something)
    else
        asd = Something - another thing
    endif
    loop
        break
        continue
    endLoop
    exit
    ; some comment sss
endProc

procedure testProcedure1(firstParam: aType) protected override
    ; some proc
    ; some comment sss
endProc

procedure testProcedure2(firstParam: aType) protected override
    ; some proc
    ; some comment sss
endProc

procedure testProcedure3(firstParam: aType) protected override
    ; some proc
    ; some comment sss
endProc



function anotherFunction return Int4
    
endfunc
    
function testFunction return Int4
    ; some function
    return 6
endFunc

procedure testProcedure(firstParam: aType) protected override
    var localVar : aType
    var var2 : int4
    ; some proc
    tetst(asd);
    if (asd + a + var2)
        writeln(something)
    else
        asd = Something - another thing
    endif
    loop
        break
        continue
    endLoop
    exit
    ; some comment sss
endProc

procedure testProcedure1(firstParam: aType) protected override
    ; some proc
    ; some comment sss
endProc

procedure testProcedure2(firstParam: aType) protected override
    ; some proc
    ; some comment sss
endProc

procedure testProcedure3(firstParam: aType) protected override
    ; some proc
    ; some comment sss
endProc



function anotherFunction return Int4
    
endfunc
    
function testFunction return Int4
    ; some function
    return 6
endFunc

procedure testProcedure(firstParam: aType) protected override
    var localVar : aType
    var var2 : int4
    ; some proc
    tetst(asd);
    if (asd + a + var2)
        writeln(something)
    else
        asd = Something - another thing
    endif
    loop
        break
        continue
    endLoop
    exit
    ; some comment sss
endProc

procedure testProcedure1(firstParam: aType) protected override
    ; some proc
    ; some comment sss
endProc

procedure testProcedure2(firstParam: aType) protected override
    ; some proc
    ; some comment sss
endProc

procedure testProcedure3(firstParam: aType) protected override
    ; some proc
    ; some comment sss
endProc



function anotherFunction return Int4
    
endfunc
    
function testFunction return Int4
    ; some function
    return 6
endFunc

procedure testProcedure(firstParam: aType) protected override
    var localVar : aType
    var var2 : int4
    ; some proc
    tetst(asd);
    if (asd + a + var2)
        writeln(something)
    else
        asd = Something - another thing
    endif
    loop
        break
        continue
    endLoop
    exit
    ; some comment sss
endProc

procedure testProcedure1(firstParam: aType) protected override
    ; some proc
    ; some comment sss
endProc

procedure testProcedure2(firstParam: aType) protected override
    ; some proc
    ; some comment sss
endProc

procedure testProcedure3(firstParam: aType) protected override
    ; some proc
    ; some comment sss
endProc



function anotherFunction return Int4
    
endfunc
    
function testFunction return Int4
    ; some function
    return 6
endFunc

procedure testProcedure(firstParam: aType) protected override
    var localVar : aType
    var var2 : int4
    ; some proc
    tetst(asd);
    if (asd + a + var2)
        writeln(something)
    else
        asd = Something - another thing
    endif
    loop
        break
        continue
    endLoop
    exit
    ; some comment sss
endProc

procedure testProcedure1(firstParam: aType) protected override
    ; some proc
    ; some comment sss
endProc

procedure testProcedure2(firstParam: aType) protected override
    ; some proc
    ; some comment sss
endProc

procedure testProcedure3(firstParam: aType) protected override
    ; some proc
    ; some comment sss
endProc



function anotherFunction return Int4
    
endfunc
    
function testFunction return Int4
    ; some function
    return 6
endFunc

procedure testProcedure(firstParam: aType) protected override
    var localVar : aType
    var var2 : int4
    ; some proc
    tetst(asd);
    if (asd + a + var2)
        writeln(something)
    else
        asd = Something - another thing
    endif
    loop
        break
        continue
    endLoop
    exit
    ; some comment sss
endProc

procedure testProcedure1(firstParam: aType) protected override
    ; some proc
    ; some comment sss
endProc

procedure testProcedure2(firstParam: aType) protected override
    ; some proc
    ; some comment sss
endProc

procedure testProcedure3(firstParam: aType) protected override
    ; some proc
    ; some comment sss
endProc
