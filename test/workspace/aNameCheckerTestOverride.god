
class aNameCheckerTestOverride(aNameCheckerTest)

wrongFieldName : CString override


proc wrongProcName override
endproc


func wrongProcName return Boolean override

endfunc

proc SomeProc(RightParamName: int, wrongParamName: int) override
endproc

