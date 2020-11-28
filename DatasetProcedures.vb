Module DatasetProcedures

    Public Function GetMaxMargin() As Double
        Dim Value As String
        For Each myRow As DataRow In myDataSet.Tables("EnvironmentVariableTable").Rows
            If myRow("Name").Trim() = "MaxMargins" Then
                Value = myRow("Value")
                Return Double.Parse(Value)
            End If
        Next
        MessageBox.Show("Shoot, I couldn't find Max Margins. Returned 0.")
        Return 0
    End Function

    Public Function GetStartDate() As Date
        Dim value As String
        For Each myRow As DataRow In myDataSet.Tables("EnvironmentVariableTable").Rows
            If myRow("Name").Trim() = "StartDate" Then
                value = myRow("Value")
                Return Date.Parse(value)
            End If
        Next
        MessageBox.Show("Holy BatScreen! Could not find 'StartDate'. Returned 1/1/1.")
        Return "1/1/1"
    End Function


    Public Function GetRiskFreeRate() As Double
        Dim Value As String
        For Each myRow As DataRow In myDataSet.Tables("EnvironmentVariableTable").Rows
            If myRow("Name").Trim() = "RiskFreeRate" Then
                Value = myRow("Value")
                Return Double.Parse(Value)
            End If
        Next
        MessageBox.Show("Shoot, I couldn't find RiskFreeRate. Returned 0.")
        Return 0

    End Function

    Public Function GetInitialCAccount() As Double
        Dim Value As String
        For Each myRow As DataRow In myDataSet.Tables("EnvironmentVariableTable").Rows
            If myRow("Name").Trim() = "CAccount" Then
                Value = myRow("Value")
                Return Double.Parse(Value)
            End If
        Next
        MessageBox.Show("Shoot, I couldn't find CAccount. Returned 0.")
        Return 0
    End Function

    Public Function GetCAccount() As Double
        Dim Value As String
        For Each myrow As DataRow In myDataSet.Tables("AcquiredPositionsTable").Rows
            If myrow("symbol").trim() = "CAccount" Then
                Value = myrow("units")
                Return Double.Parse(Value)
            End If

        Next
        MessageBox.Show("Zoinks! I couldn't find 'CAccount'. Returned 0.")
        Return 0
    End Function


    Public Function IsAStock(symbol As String) As Boolean
        symbol = symbol.Trim()
        For Each myRow As DataRow In myDataSet.Tables("TickerTable").Rows
            If myRow("Ticker").trim() = symbol Then
                Return True
            End If
        Next
        Return False
    End Function

    Public Function GetAsk(symbol As String, targetDate As Date) As Double
        symbol = symbol.Trim()
        If (targetDate.DayOfWeek = DayOfWeek.Saturday) Then
            targetDate = targetDate.AddDays(-1)
        End If
        If (targetDate.DayOfWeek = DayOfWeek.Sunday) Then
            targetDate = targetDate.AddDays(-2)
        End If

        DownloadPricesForOneDay(targetDate)

        If IsAStock(symbol) Then
            For Each myRow As DataRow In myDataSet.Tables("StockMarketOneDayTable").Rows
                If myRow("Ticker").trim() = symbol Then
                    Return myRow("Ask")
                End If
            Next
        Else ' is an option
            For Each myRow As DataRow In myDataSet.Tables("OptionMarketOneDayTable").Rows
                If myRow("Symbol").trim() = symbol Then
                    Return myRow("Ask")
                End If
            Next
        End If
        MessageBox.Show("Darn! Could not find the ask for " + symbol + ". Returned 0.")
        Return 0
    End Function

    Public Function GetBid(symbol As String, targetDate As Date) As Double
        symbol = symbol.Trim()
        If (targetDate.DayOfWeek = DayOfWeek.Saturday) Then
            targetDate = targetDate.AddDays(-1)
        End If
        If (targetDate.DayOfWeek = DayOfWeek.Sunday) Then
            targetDate = targetDate.AddDays(-2)
        End If

        DownloadPricesForOneDay(targetDate)

        If IsAStock(symbol) Then
            For Each myRow As DataRow In myDataSet.Tables("StockMarketOneDayTable").Rows
                If myRow("Ticker").trim() = symbol Then
                    Return myRow("Bid")
                End If
            Next
        Else ' is an option
            For Each myRow As DataRow In myDataSet.Tables("OptionMarketOneDayTable").Rows
                If myRow("Symbol").trim() = symbol Then
                    Return myRow("Bid")
                End If
            Next
        End If
        MessageBox.Show("Darn! Could not find the bid for " + symbol + ". Returned 0.")
        Return 0
    End Function

    Public Function GetDividend(ticker As String, targetDate As Date) As Double
        If IsAStock(ticker) Then
            If (targetDate.DayOfWeek = DayOfWeek.Saturday) Then
                targetDate = targetDate.AddDays(-1)
            End If
            If (targetDate.DayOfWeek = DayOfWeek.Sunday) Then
                targetDate = targetDate.AddDays(-2)
            End If

            DownloadPricesForOneDay(targetDate)

            For Each myRow As DataRow In myDataSet.Tables("StockMarketOneDayTable").Rows
                If myRow("Ticker").trim() = ticker Then
                    Return Double.Parse(myRow("Dividend"))
                End If
            Next
        End If
        MessageBox.Show("Holy Batshoelace! I could not find the dividend for " + ticker + ". Returned 0.")
        Return 0
    End Function

    Public Function GetStrike(symbol As String)
        For Each myRow As DataRow In myDataSet.Tables("OptionMarketOneDayTable").Rows
            If myRow("Symbol").trim() = symbol Then
                Return Double.Parse(myRow("Strike"))
            End If
        Next
        MessageBox.Show("Holy Batshoelace! I could not find the strike for " + symbol + ". Returned 0.")
        Return 0
    End Function

    Public Function GetTrCostCoefficient(secType As String, trType As String) As Double
        For Each myRow As DataRow In myDataSet.Tables("TransactionCostTable").Rows
            If myRow("SecurityType").trim() = secType And myRow("TransactionType").trim() = trType Then
                Return Double.Parse(myRow("CostCoeff"))
            End If
        Next
        MessageBox.Show("Holy Batshoelace! I could not find the transaction cost. Returned 0.")
        Return 0
    End Function

    Public Function GetCurrentPositionInAP(symbol) As Double
        For Each myRow As DataRow In myDataSet.Tables("AcquiredPositionsTable").Rows
            If myRow("symbol").ToString().Trim() = symbol Then
                Return Double.Parse(myRow("Units"))
            End If
        Next
        Return 0
    End Function

    Public Function GetUnderlier(symbol As String) As String
        symbol = symbol.Trim()
        For Each myRow As DataRow In myDataSet.Tables("OptionMarketOneDayTable").Rows
            If myRow("Symbol").trim() = symbol Then
                Return myRow("Underlier").Trim()
            End If
        Next
        MessageBox.Show("Holy BatCucumber! I could not find the underlier for " + symbol + ". Returned ???")
        Return "???"
    End Function

    Public Function IsACall(symbol As String) As Boolean
        symbol = symbol.Trim()
        For Each myRow As DataRow In myDataSet.Tables("OptionMarketOneDayTable").Rows
            If myRow("Symbol").trim() = symbol And myRow("Type").trim() = "Call" Then
                Return True
            End If
        Next
        Return False
    End Function

    Public Function IsAPut(symbol As String) As Boolean
        symbol = symbol.Trim()
        For Each myRow As DataRow In myDataSet.Tables("OptionMarketOneDayTable").Rows
            If myRow("Symbol").trim() = symbol And myRow("Type").trim() = "Put" Then
                Return True
            End If
        Next
        Return False
    End Function
End Module
