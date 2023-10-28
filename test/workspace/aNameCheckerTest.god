
class aNameCheckerTest(aRootClass)

const cRightConstantName = 'something'
const pWrongConstantName = 1

type tRightTypeName : int
type xWrongTypeName : Something

RightFieldName : Boolean
wrongFieldName : CString


proc RightProcName
endproc

proc wrongProcName
endproc

func RightProcName return Boolean
endfunc

func wrongProcName return Boolean

endfunc

proc SomeProc(RightParamName: int, wrongParamName: int)
   self.
endproc

