

procedure ReadBinary(Offset : tShort, SFI : tByte, Addr : tShort, Length : tByte)
   var Cmd : tAPDUCommand
   var P1 : tByte
   var P2 : tByte
   
   P1 = OcsPrimaryTypes.GetHighByte(Offset)
   P2 = OcsPrimaryTypes.GetLowByte(Offset)
   Cmd = self.MyModel.ISOAPDUReadBinary(P1, P2, Length)
   self.PersoRuntimeParameters.AddCommand(Cmd)
endProc 


