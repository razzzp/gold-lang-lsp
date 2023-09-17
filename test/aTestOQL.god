

proc TestOQL
    forEach curPerson in OQL
    select * from x in aPerson++ where x.LastName like 'S'
        WriteLn(curPerson)
    endFor

    theCursor = Motor.OpenOQLCursor
    OQL select OQLMax(thePers.Salary), OQLMin(thePers.Salary), OQLSum(thePers.Salary),
        OQLCount(*) from thePers in aPerson where thePers.Division = ForDivision using theCursor
    Error = Motor.OQLError
    if Error = 0
        OQL Fetch into MaxPay, MinPay, TotalPay, NumEmployees using theCursor
    else
        Alert('OQL Select error ' + IaS(Error))
    endIf
    Motor.CloseOQLCursor(theCursor)

    forEach curCompany in OQL select * from x in aCompany where (x.Name like self.Name) and
        OQLConditionalWhere(self.City <> '', x.City = self.City) and
        OQLConditionalWhere(self.SearchByManpower, (x.ManPower >= self.MinNumEmployees) and (x.ManPower <= self.MaxNumEmployees)) and
        OQLConditionalWhere(self.SearchByValue, (x.CompanyValue >= self.MinValue) and (x.CompanyValue <= self.MaxValue))
        ; do something
    endFor

    forEach curCompany in OQL select * from x in aCompany where x.City like self.City order by x.CompanyValue descending, x.ManPower
        ; do something
    endFor

    forEach curInfo in OQL select x.LastName, y.LastName from x in aPerson, y in aTeacher where y.myStudents.ContainsObject(x)
        WriteLn(curInfo.x.LastName, ' is in class taught by ', curInfo.y.LastName)
    endFor

    if not Motor.IsNew(self)
      forEach curSmallId in OQL Select top 1 x.NSId, x.Id, x.Version from x in aEntity++
         where (x.Somethin.NSId = self.NSId) and (x.Something.Id = 
         self.Id)
         _Result = True
      endFor
   endIf
endproc