class aTestClass (aFullObject)

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

type tInstanceType instanceOf aInstance
