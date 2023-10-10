class aRootClass 

const cRootConstant = 'adasdsadas'

type tRootRecordType: record
   field1: Int4
   field2: CString
endrecord

RootVar1 : CString
RootVarRef2 : refto aLightObject


proc RootProc(FirstParam: Int4)
   var localVar : CString
   ;proc body
   WriteLn(FirstParam)
   WriteLn(localVar)
endProc