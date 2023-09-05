

proc Test2 

   efPossiblePatternList.initForRecords(-1, sizeof(CString))
   context.RegisterStyle(cTTCFooter, 0, cDCBlack, 0, [], cDGSFNArial, 10, cDGPARight)
   self.UpdateCodeStatusFromCodeBytes(self.PersoRuntimeParameters.CodeStatus[scPIN1], 
         CodeByte)
   OcsPersoPerfAnalyzerUtil.SetCurGlobalDebugTypeInfo(oldDebugTypeInfo + [cMemoryInfo])
endproc