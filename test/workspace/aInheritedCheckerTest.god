
class aInheritedCheckerTest(aRootClass)

procedure Init override
   ; case called
   inherited self.Init
endProc

procedure Terminate override
   ; case not called
endProc

procedure NotifyInit override
   ; case pass
   pass
endProc