Public Class Form1
    Dim E1(0, 3) As Integer
    Dim E2(0, 3) As Integer
    Dim E3(0, 3) As Integer
    Dim ET1(3, 0) As Integer
    Dim ET2(3, 0) As Integer
    Dim Mi(3, 3) As Integer
    Dim Temp1(3, 3) As Integer
    Dim Temp2(3, 3) As Integer
    Dim W1(3, 3) As Integer
    Dim W2(3, 3) As Integer
    Dim Wap(3, 3) As Integer
    Dim Ys3(0, 3) As Integer
    Dim Ys(0, 3) As Integer
    Dim Yo(0, 3) As Integer
    Dim i, j, fila, columna As Integer
    Private Sub btnAnalizar_Click(sender As Object, e As EventArgs) Handles btnAnalizar.Click
        txtPatronRsp.Text = "          A"
        Call llenaDatos()
        Call transpuesta()
        Call multiplicaT()
        Call resta()
        Call Funcion_Wap()
        Call Hallar_Ys3()
        Call Hallar_Yo()
        Call ComprobarAprendizaje()

    End Sub
    Private Sub llenaDatos()
        'Para el vector E1
        E1(0, 0) = 1
        E1(0, 1) = 1
        E1(0, 2) = -1
        E1(0, 3) = -1
        txtE1.Text = "[" & E1(0, 0) & "  " & E1(0, 1) & "  " & E1(0, 2) & "  " & E1(0, 3) & "]"
        'Para el vector E2
        E2(0, 0) = -1
        E2(0, 1) = -1
        E2(0, 2) = 1
        E2(0, 3) = 1
        txtE2.Text = "[" & E2(0, 0) & "  " & E2(0, 1) & "  " & E2(0, 2) & "  " & E2(0, 3) & "]"
        'Para el vector E3
        E3(0, 0) = 1
        E3(0, 1) = -1
        E3(0, 2) = -1
        E3(0, 3) = -1
        'MATRIZ IDENTIDAD
        'Mi(0, 0) = 1
        'Mi(0, 1) = 0
        'Mi(0, 2) = 0
        'Mi(0, 3) = 0
        'Mi(1, 0) = 0
        'Mi(1, 1) = 1
        'Mi(1, 2) = 0
        'Mi(1, 3) = 0
        'Mi(2, 0) = 0
        'Mi(2, 1) = 0
        'Mi(2, 2) = 1
        'Mi(2, 3) = 0
        'Mi(3, 0) = 0
        'Mi(3, 1) = 0
        'Mi(3, 2) = 0
        'Mi(3, 3) = 1
        For i = 0 To 3
            For j = 0 To 3
                If i = j Then
                    Mi(i, j) = 1
                Else
                    Mi(i, j) = 0
                End If
            Next
        Next
    End Sub
    Private Sub transpuesta()
        'Para el vector TE1
        ET1(0, 0) = E1(0, 0)
        ET1(1, 0) = E1(0, 1)
        ET1(2, 0) = E1(0, 2)
        ET1(3, 0) = E1(0, 3)
        'Para el vector TE2
        ET2(0, 0) = E2(0, 0)
        ET2(1, 0) = E2(0, 1)
        ET2(2, 0) = E2(0, 2)
        ET2(3, 0) = E2(0, 3)
    End Sub
    Private Sub multiplicaT()
        'MATRIZ Temporal 1
        Temp1(0, 0) = ET1(0, 0) * E1(0, 0)
        Temp1(0, 1) = ET1(0, 0) * E1(0, 1)
        Temp1(0, 2) = ET1(0, 0) * E1(0, 2)
        Temp1(0, 3) = ET1(0, 0) * E1(0, 3)
        Temp1(1, 0) = ET1(1, 0) * E1(0, 0)
        Temp1(1, 1) = ET1(1, 0) * E1(0, 1)
        Temp1(1, 2) = ET1(1, 0) * E1(0, 2)
        Temp1(1, 3) = ET1(1, 0) * E1(0, 3)
        Temp1(2, 0) = ET1(2, 0) * E1(0, 0)
        Temp1(2, 1) = ET1(2, 0) * E1(0, 1)
        Temp1(2, 2) = ET1(2, 0) * E1(0, 2)
        Temp1(2, 3) = ET1(2, 0) * E1(0, 3)
        Temp1(3, 0) = ET1(3, 0) * E1(0, 0)
        Temp1(3, 1) = ET1(3, 0) * E1(0, 1)
        Temp1(3, 2) = ET1(3, 0) * E1(0, 2)
        Temp1(3, 3) = ET1(3, 0) * E1(0, 3)
        'For i = 0 To 3
        '    For j = 0 To 3
        '        Temp1(i, j) = ET1(i, j) * E1(i, j)
        '    Next
        'Next
        'MATRIZ Temporal 2
        Temp2(0, 0) = ET2(0, 0) * E2(0, 0)
        Temp2(0, 1) = ET2(0, 0) * E2(0, 1)
        Temp2(0, 2) = ET2(0, 0) * E2(0, 2)
        Temp2(0, 3) = ET2(0, 0) * E2(0, 3)
        Temp2(1, 0) = ET2(1, 0) * E2(0, 0)
        Temp2(1, 1) = ET2(1, 0) * E2(0, 1)
        Temp2(1, 2) = ET2(1, 0) * E2(0, 2)
        Temp2(1, 3) = ET2(1, 0) * E2(0, 3)
        Temp2(2, 0) = ET2(2, 0) * E1(0, 0)
        Temp2(2, 1) = ET2(2, 0) * E2(0, 1)
        Temp2(2, 2) = ET2(2, 0) * E2(0, 2)
        Temp2(2, 3) = ET2(2, 0) * E2(0, 3)
        Temp2(3, 0) = ET2(3, 0) * E2(0, 0)
        Temp2(3, 1) = ET2(3, 0) * E2(0, 1)
        Temp2(3, 2) = ET2(3, 0) * E2(0, 2)
        Temp2(3, 3) = ET2(3, 0) * E2(0, 3)
    End Sub
    Private Sub resta()
        'MATRIZ w1
        W1(0, 0) = Temp1(0, 0) - Mi(0, 0)
        W1(0, 1) = Temp1(0, 1) - Mi(0, 1)
        W1(0, 2) = Temp1(0, 2) - Mi(0, 2)
        W1(0, 3) = Temp1(0, 3) - Mi(0, 3)
        W1(1, 0) = Temp1(1, 0) - Mi(1, 0)
        W1(1, 1) = Temp1(1, 1) - Mi(1, 1)
        W1(1, 2) = Temp1(1, 2) - Mi(1, 2)
        W1(1, 3) = Temp1(1, 3) - Mi(1, 3)
        W1(2, 0) = Temp1(2, 0) - Mi(2, 0)
        W1(2, 1) = Temp1(2, 1) - Mi(2, 1)
        W1(2, 2) = Temp1(2, 2) - Mi(2, 2)
        W1(2, 3) = Temp1(2, 3) - Mi(2, 3)
        W1(3, 0) = Temp1(3, 0) - Mi(3, 0)
        W1(3, 1) = Temp1(3, 1) - Mi(3, 1)
        W1(3, 2) = Temp1(3, 2) - Mi(3, 2)
        W1(3, 3) = Temp1(3, 3) - Mi(3, 3)
        'MATRIZ w2
        W2(0, 0) = Temp2(0, 0) - Mi(0, 0)
        W2(0, 1) = Temp2(0, 1) - Mi(0, 1)
        W2(0, 2) = Temp2(0, 2) - Mi(0, 2)
        W2(0, 3) = Temp2(0, 3) - Mi(0, 3)
        W2(1, 0) = Temp2(1, 0) - Mi(1, 0)
        W2(1, 1) = Temp2(1, 1) - Mi(1, 1)
        W2(1, 2) = Temp2(1, 2) - Mi(1, 2)
        W2(1, 3) = Temp2(1, 3) - Mi(1, 3)
        W2(2, 0) = Temp2(2, 0) - Mi(2, 0)
        W2(2, 1) = Temp2(2, 1) - Mi(2, 1)
        W2(2, 2) = Temp2(2, 2) - Mi(2, 2)
        W2(2, 3) = Temp2(2, 3) - Mi(2, 3)
        W2(3, 0) = Temp2(3, 0) - Mi(3, 0)
        W2(3, 1) = Temp2(3, 1) - Mi(3, 1)
        W2(3, 2) = Temp2(3, 2) - Mi(3, 2)
        W2(3, 3) = Temp2(3, 3) - Mi(3, 3)
    End Sub
    Private Sub Funcion_Wap()
        For i = 0 To 3
            For j = 0 To 3
                Wap(i, j) = W1(i, j) + W2(i, j)

            Next
        Next
    End Sub
    Private Sub Hallar_Ys3()
        Ys3(0, 0) = E3(0, 0) * Wap(0, 0) + E3(0, 1) * Wap(1, 0) + E3(0, 2) * Wap(2, 0) + E3(0, 3) * Wap(3, 0)
        Ys3(0, 1) = E3(0, 0) * Wap(0, 1) + E3(0, 1) * Wap(1, 1) + E3(0, 2) * Wap(2, 1) + E3(0, 3) * Wap(3, 1)
        Ys3(0, 2) = E3(0, 0) * Wap(0, 2) + E3(0, 1) * Wap(1, 2) + E3(0, 2) * Wap(2, 2) + E3(0, 3) * Wap(3, 3)
        Ys3(0, 3) = E3(0, 0) * Wap(0, 3) + E3(0, 1) * Wap(1, 3) + E3(0, 2) * Wap(2, 3) + E3(0, 3) * Wap(3, 3)
        txtYs3.Text = "[" & Ys3(0, 0) & "  " & Ys3(0, 1) & "  " & Ys3(0, 2) & "  " & Ys3(0, 3) & "]"
    End Sub
    Private Sub Hallar_Yo()
        If Ys3(0, 0) >= 0 Then
            Yo(0, 0) = 1
        Else
            Yo(0, 0) = -1
        End If
        If Ys3(0, 1) >= 0 Then
            Yo(0, 1) = 1
        Else
            Yo(0, 1) = -1
        End If
        If Ys3(0, 2) >= 0 Then
            Yo(0, 2) = 1
        Else
            Yo(0, 2) = -1
        End If
        If Ys3(0, 3) >= 0 Then
            Yo(0, 3) = 1
        Else
            Yo(0, 3) = -1
        End If
        txtYo.Text = "[" & Yo(0, 0) & "  " & Yo(0, 1) & "  " & Yo(0, 2) & "  " & Yo(0, 3) & "]"
    End Sub
    Private Sub ComprobarAprendizaje()
        Ys(0, 0) = Yo(0, 0) * Wap(0, 0) + Yo(0, 1) * Wap(1, 0) + Yo(0, 2) * Wap(2, 0) + Yo(0, 3) * Wap(3, 0)
        Ys(0, 1) = Yo(0, 0) * Wap(0, 1) + Yo(0, 1) * Wap(1, 1) + Yo(0, 2) * Wap(2, 1) + Yo(0, 3) * Wap(3, 1)
        Ys(0, 2) = Yo(0, 0) * Wap(0, 2) + Yo(0, 1) * Wap(1, 2) + Yo(0, 2) * Wap(2, 2) + Yo(0, 3) * Wap(3, 3)
        Ys(0, 3) = Yo(0, 0) * Wap(0, 3) + Yo(0, 1) * Wap(1, 3) + Yo(0, 2) * Wap(2, 3) + Yo(0, 3) * Wap(3, 3)
        txtYs.Text = "[" & Ys(0, 0) & "  " & Ys(0, 1) & "  " & Ys(0, 2) & "  " & Ys(0, 3) & "]"
    End Sub
    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
    End Sub
End Class
