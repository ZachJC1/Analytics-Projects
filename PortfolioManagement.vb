Module PortfolioManagement

    Public Function CalcMTM(symbol As String, targetDate As Date) As Double
        If symbol = "CAccount" Then
            Return 1
        Else
            Return (GetAsk(symbol, targetDate) + GetBid(symbol, targetDate)) / 2
        End If
    End Function

    Public Function CalcAPValue(targetDate As Date) As Double
        Dim cumulativeAPValue As Double = 0
        Dim symbol As String
        Dim units As Double
        Dim posValue As Double

        For Each myRow As DataRow In myDataSet.Tables("AcquiredPositionsTable").Rows
            symbol = myRow("Symbol").ToString().Trim
            units = myRow("Units")
            If symbol <> "CAccount" Then
                posValue = units * CalcMTM(symbol, targetDate)
                cumulativeAPValue = cumulativeAPValue + posValue
                myRow("Value") = posValue
            End If
        Next
        Return cumulativeAPValue
    End Function

    Public Function CalcIPValue(targetDate As Date) As Double
        Dim cumulativeIPValue As Double = 0
        Dim symbol As String
        Dim units As Double
        Dim posValue As Double

        For Each myRow As DataRow In myDataSet.Tables("InitialPositionsTable").Rows
            symbol = myRow("Symbol").ToString().Trim
            units = myRow("Units")
            If symbol <> "CAccount" Then
                posValue = units * CalcMTM(symbol, targetDate)
                cumulativeIPValue = cumulativeIPValue + posValue
                myRow("Value") = posValue
            End If
        Next
        Return cumulativeIPValue
    End Function

    Public Function CalcMargin(targetDate As Date) As Double
        Return CalcAPMargin(targetDate) + CalcIPMargin(targetDate)
    End Function

    Public Function CalcAPMargin(targetDate As Date) As Double
        Dim cumulativeAPMValue As Double = 0
        Dim symbol As String
        Dim units As Double
        Dim posValue As Double
        For Each myRow As DataRow In myDataSet.Tables("AcquiredPositionsTable").Rows
            symbol = myRow("Symbol").ToString().Trim
            units = myRow("Units")
            If symbol <> "CAccount" And units < 0 Then
                posValue = units * CalcMTM(symbol, targetDate)
                cumulativeAPMValue = cumulativeAPMValue + posValue
            End If
        Next
        Return cumulativeAPMValue
    End Function

    Public Function CalcIPMargin(targetDate As Date) As Double
        Dim cumulativeIPMValue As Double = 0
        Dim symbol As String
        Dim units As Double
        Dim posValue As Double
        For Each myRow As DataRow In myDataSet.Tables("InitialPositionsTable").Rows
            symbol = myRow("Symbol").ToString().Trim
            units = myRow("Units")
            If symbol <> "CAccount" And units < 0 Then
                posValue = units * CalcMTM(symbol, targetDate)
                cumulativeIPMValue = cumulativeIPMValue + posValue
            End If
        Next
        Return cumulativeIPMValue
    End Function

    Public Function CalcTPVatStart() As Double
        Return CalcIPValue(startDate) + initialCAccount
    End Function

    Public Function CalcTaTPV(targetDate As Date) As Double
        Dim ts As TimeSpan = targetDate.Date - startDate.Date
        Dim t As Double = ts.Days / 365.25
        Return TPVatStart * Math.Exp(riskFreeRate * t)
    End Function

    Public Function IsInIP(x As String) As Boolean
        x = x.Trim()
        For Each myRow As DataRow In myDataSet.Tables("InitialPositionsTable").Rows
            If myRow("Symbol").trim() = x Then
                Return True
            End If
        Next
        Return False
    End Function

    Public Function CalcTPV(currentDate) As Double
        Return IPValue + APValue + CAccount + CalcInterestSLT(currentDate)
    End Function

    Public Function CalcInterestSLT(toThisDay As Date) As Double
        Dim ts As TimeSpan = toThisDay.Date - lastTransactionDate.Date
        Dim t As Double = ts.Days / 365.25
        Return CAccount * (Math.Exp(riskFreeRate * t) - 1)
    End Function

    Public Function CalcTE() As Double
        If TPV >= TaTPV Then
            Return (TPV - TaTPV) * 0.25
        Else
            Return TaTPV - TPV
        End If
    End Function

    Public Function UpdateSumTE(targetDate As Date) As Double
        If targetDate.DayOfWeek = DayOfWeek.Sunday And targetDate.Date > lastTEUpdate.Date Then
            lastTEUpdate = targetDate
            Return TE
        Else
            Return 0
        End If
    End Function

    Public Sub Execute(t As Transaction)
        Dim mySQL As String
        mySQL = String.Format("INSERT INTO TransactionQueue (Date, TeamID, Symbol, Type, Qty, Price, Cost, TotValue, " +
                              "InterestSinceLastTransaction, CashPositionAfterTransaction, TotMargin) VALUES " + "('{0}', {1}, '{2}', '{3}', {4}, {5}, {6}, {7}, {8}, {9}, {10})",
                              currentDate.ToShortDateString(),
                              TeamID,
                              t.symbol,
                              t.type,
                              t.qty,
                              t.price,
                              t.tcost,
                              t.totValue,
                              t.interestSLT,
                              t.CAccountAT,
                              t.marginAT)
        RunNonQuery(mySQL)
        lastTransactionDate = currentDate
        CAccount = t.CAccountAT
        margin = t.marginAT
        'UpdatePosition(t.type, t.symbol, t.qty)
    End Sub
End Module
