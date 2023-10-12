class aRootClass 

uses aSecondClass

const cRootConstant = 'adasdsadas'

type tRootRecordType: record
   field1: Int4
   field2: CString
endrecord

RootVar1 : CString
RootVarRef2 : refto aLightObject


proc RootProc(FirstParam: Int4)
   var localVar : CString
   var secondLocaVar : aSecondClass
   ;
   ;proc body
   WriteLn(FirstParam)
   WriteLn(localVar)
   ;
   WriteLn(secondLocaVar.secondClassField1)
   WriteLn(self.RootVar1)
endProc


proc SecondRootProc(FirstParam: Int4)
   var localVar : CString
   var same_class : aRootClass
   ;
   ;proc body
   WriteLn(FirstParam)
   WriteLn(localVar)
   ;
   WriteLn(same_class.RootVarRef2)
   WriteLn(self.RootVar1)
endProc