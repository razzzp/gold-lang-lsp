class aThirdClass(aRootClass)

proc ThirdClassProc
endProc

func ThirdClassFunc return aRootClass
endfunc

procedure ThirdClassTestProc
   ; test
   self.ThirdClassProc
   self.ThirdClassFunc.RootVar1
endProc
