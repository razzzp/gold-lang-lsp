class aFifthClass(aFourthClass)

uses aModule

secondClassField1 : CString override
secondClassField2 : Int4 override

procedure FifthClassProc
   ; test
   self.secondClassField1
   self.secondClassField2
   aModule.
endProc

procedure FourthClassProc override
   ; test
endProc
