VERSION 5.00
Begin VB.Form Post 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Post-Procesador"
   ClientHeight    =   6105
   ClientLeft      =   90
   ClientTop       =   390
   ClientWidth     =   9450
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   6105
   ScaleWidth      =   9450
   Begin VB.CheckBox Check6 
      Caption         =   "Check5"
      Height          =   195
      Left            =   8520
      TabIndex        =   23
      Top             =   3000
      Width           =   195
   End
   Begin VB.CheckBox Check5 
      Caption         =   "Check5"
      Height          =   195
      Left            =   8520
      TabIndex        =   22
      Top             =   2640
      Width           =   195
   End
   Begin VB.PictureBox Picture2 
      AutoRedraw      =   -1  'True
      BackColor       =   &H00FFFFFF&
      Height          =   5955
      Left            =   6000
      ScaleHeight     =   393
      ScaleMode       =   3  'Píxel
      ScaleWidth      =   81
      TabIndex        =   21
      Top             =   60
      Width           =   1275
   End
   Begin VB.CheckBox Check4 
      Caption         =   "Vectores de velocidades"
      Height          =   255
      Left            =   7380
      TabIndex        =   16
      Top             =   4260
      Width           =   2715
   End
   Begin VB.CheckBox Check3 
      Caption         =   "Lineas de corrientes"
      Height          =   195
      Left            =   7380
      TabIndex        =   15
      Top             =   3660
      Width           =   2715
   End
   Begin VB.TextBox Text5 
      Height          =   285
      Left            =   8880
      TabIndex        =   14
      Text            =   "10"
      Top             =   3300
      Width           =   495
   End
   Begin VB.CheckBox Check2 
      Caption         =   "Isotérmas"
      Height          =   195
      Left            =   7380
      TabIndex        =   13
      Top             =   2280
      Width           =   2655
   End
   Begin VB.CheckBox Check1 
      Caption         =   "Contour"
      Height          =   195
      Left            =   7380
      TabIndex        =   12
      Top             =   1980
      Width           =   2595
   End
   Begin VB.TextBox Text4 
      Height          =   285
      Left            =   8880
      ScrollBars      =   1  'Horizontal
      TabIndex        =   11
      Top             =   2940
      Width           =   495
   End
   Begin VB.TextBox Text3 
      Height          =   285
      Left            =   8880
      ScrollBars      =   1  'Horizontal
      TabIndex        =   10
      Top             =   2580
      Width           =   495
   End
   Begin VB.CommandButton Command5 
      Caption         =   "Planos"
      Height          =   375
      Left            =   7800
      TabIndex        =   9
      Top             =   1020
      Width           =   975
   End
   Begin VB.TextBox Text2 
      Height          =   285
      Left            =   8880
      TabIndex        =   8
      Text            =   "0.01"
      Top             =   4620
      Width           =   495
   End
   Begin VB.CommandButton Command3 
      Caption         =   "Calcular"
      Height          =   375
      Left            =   7800
      TabIndex        =   5
      Top             =   60
      Width           =   975
   End
   Begin VB.TextBox Text1 
      Height          =   285
      Left            =   8880
      TabIndex        =   4
      Text            =   "10"
      Top             =   3900
      Width           =   495
   End
   Begin VB.CommandButton Command2 
      Caption         =   "Salir"
      Height          =   375
      Left            =   7800
      TabIndex        =   2
      Top             =   1500
      Width           =   975
   End
   Begin VB.CommandButton Command1 
      Caption         =   "Dibujar"
      Height          =   375
      Left            =   7800
      TabIndex        =   1
      Top             =   540
      Width           =   975
   End
   Begin VB.PictureBox Picture1 
      AutoRedraw      =   -1  'True
      BackColor       =   &H00FFFFFF&
      FillColor       =   &H00FFFFFF&
      ForeColor       =   &H00000000&
      Height          =   5955
      Left            =   60
      ScaleHeight     =   -1.091
      ScaleLeft       =   -0.05
      ScaleMode       =   0  'Usuario
      ScaleTop        =   1.05
      ScaleWidth      =   1.092
      TabIndex        =   0
      Top             =   60
      Width           =   5955
   End
   Begin VB.Label Label7 
      Caption         =   "Factor de Escala"
      Height          =   195
      Left            =   7560
      TabIndex        =   20
      Top             =   4680
      Width           =   1215
   End
   Begin VB.Label Label6 
      Caption         =   "Intervalos"
      Height          =   195
      Left            =   7500
      TabIndex        =   19
      Top             =   3360
      Width           =   1215
   End
   Begin VB.Label Label5 
      Caption         =   "T Minima (x)"
      Height          =   195
      Left            =   7500
      TabIndex        =   18
      Top             =   3000
      Width           =   1215
   End
   Begin VB.Label Label2 
      Caption         =   "T Máxima (+)"
      Height          =   195
      Left            =   7500
      TabIndex        =   17
      Top             =   2640
      Width           =   1215
   End
   Begin VB.Label Label4 
      AutoSize        =   -1  'True
      Height          =   195
      Left            =   7440
      TabIndex        =   7
      Top             =   5400
      Width           =   45
   End
   Begin VB.Label Label3 
      AutoSize        =   -1  'True
      Height          =   195
      Left            =   7440
      TabIndex        =   6
      Top             =   5040
      Width           =   45
   End
   Begin VB.Label Label1 
      Caption         =   "No de lineas"
      Height          =   195
      Left            =   7500
      TabIndex        =   3
      Top             =   3960
      Width           =   975
   End
End
Attribute VB_Name = "Post"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False

Sub Bo()
    Picture1.Cls
    Picture1.DrawWidth = 1
    Picture1.DrawStyle = 0
    For i = 1 To L1
        For j = 1 To M1
            Picture1.PSet (x(i), y(j)), QBColor(0)
        Next j
    Next i
    Picture1.FillStyle = 1
    For i = 1 To Ns
        Picture1.Line (S(0, i), S(1, i))-(S(2, i), S(3, i)), QBColor(14), B
    Next i
End Sub

Sub CargaA()
'Carga las coodenadas de los solidos y sus propiedades
    Open "solido.el" For Input As #1
        Input #1, Ns
        ReDim S(6, Ns)
        For i = 1 To Ns
            Input #1, S(0, i), S(1, i), S(2, i), S(3, i), S(4, i), S(5, i), S(6, i)
        Next i
        Input #1, Km
    Close #1
'Carga el tipo de coodenadas a utilizada y las coodenadas de la malla
    Open "geom.el" For Input As #1
        Input #1, Mode
        Input #1, Xl, Yl, R1
        Input #1, L1, M1
        Solve = ""
        Input #1, Solve
        M2 = M1 - 1
        M3 = M2 - 1
        L2 = L1 - 1
        L3 = L2 - 1
       
        ReDim x(L1), y(M1), Xu(L1), Yv(M1)
        ReDim T(L1, M1), P(L1, M1), Pc(L1, M1), u(L1, M1), v(L1, M1) ', F(L1, M1)
        ReDim Velocidad(L1, M1)
        For i = 2 To L1
            Input #1, Xu(i)
        Next i
        For j = 2 To M1
            Input #1, Yv(j)
        Next j
        Input #1, Simetria
    Close #1
    
   
'Carga el campo de Temperatura
    Open "t.el" For Input As #1
        For j = 1 To M1
            For i = 1 To L1
                Input #1, T(i, j)
            Next i
        Next j
    Close #1
    Call Cadicio
'Carga el campo de velocidades en la direccion Y
    Open "v.el" For Input As #1
        For j = 2 To M1
            For i = 1 To L1
                Input #1, v(i, j)
            Next i
        Next j
    Close #1
'Carga el campo de velocidades en la direccion X
    Open "u.el" For Input As #1
        For j = 1 To M1
            For i = 2 To L1
                Input #1, u(i, j)
            Next i
        Next j
    Close #1
'Carga el campo de Presiones
    Open "p.el" For Input As #1
        For j = 1 To M1
            For i = 1 To L1
                Input #1, P(i, j)
            Next i
        Next j
    Close #1
'Carga el campo para las lineas de corrientes
    Open "pc.el" For Input As #1
        For j = 2 To M1
            For i = 2 To L1
                Input #1, Pc(i, j)
            Next i
        Next j
    Close #1
    For i = 0 To 2
        eee = Mid(Solve, (i * 2 + 1), 1)
        If eee = "F" Then
            Select Case i
                Case 0, 1
                     Check3.Enabled = False
                     Check4.Enabled = False
                     Planos.Option1(1).Enabled = False
                     Planos.Option1(2).Enabled = False
                 Case 2
                     Check1.Enabled = False
                     Check2.Enabled = False
                     Planos.Option1(0).Enabled = False
            End Select
        Else
            Select Case i
                Case 0, 1
                     Check3.Enabled = True
                     Check4.Enabled = True
                     Planos.Option1(1).Enabled = True
                     Planos.Option1(2).Enabled = True
                Case 2
                     Check1.Enabled = True
                     Check2.Enabled = True
                     Planos.Option1(0).Enabled = True
            End Select
        End If
    Next i
    Select Case Mode
        Case 1
            Label3 = "Coordenadas Cartesianas"
            Label4 = "Xl= " & Xl & " Yl= " & Yl
        Case 2
            Label3 = "Coordenadas Axisimétricas"
            Label4 = "Xl= " & Xl & " Yl= " & Yl & " R(1)= " & R1
    End Select
        cMax = FMax(T)
        cMin = FMin(T)
        Picture1.DrawStyle = 0
        c = 255
        Coc = cMax
        If cMax = 0 Or cMin = 0 Then
            Ll = cMax - cMin > 0.0001
        ElseIf Fix(Log10(Abs(cMax))) = 0 Or Fix(Log10(Abs(cMin))) = 0 Then
                Ll = cMax - cMin > 0.0001
        Else
            Ll = ((cMax / Fix(Log10(Abs(cMax))) - cMin / Fix(Log10(Abs(cMin)))) > 0.0001)
        End If
        If Not (Ll) Then
            Check2.Enabled = False
            Check5.Enabled = False
            Check6.Enabled = False
        End If
End Sub
Sub Cadicio()
    L2 = L1 - 1
    M2 = M1 - 1
    L3 = L2 - 1
    M3 = M2 - 1
    x(L1) = Xl
    y(M1) = Yl
    For i = 2 To L2
        x(i) = (Xu(i + 1) + Xu(i)) / 2
    Next i
    For j = 2 To M2
        y(j) = (Yv(j + 1) + Yv(j)) / 2
    Next j

   
   
End Sub




Function ToPixelX(Xf) As Long
Static Fx, We, Wp, Xe
Me.ScaleMode = vbPixels
We = Picture1.ScaleWidth
Xe = Picture1.ScaleLeft
Wp = Picture1.Width
Fx = Wp / We

ToPixelX = (Fx * (Xf - Xe))
ToPixelX = Picture1.ScaleX(Xf - Xe, 0, 3)


End Function

Function ToPixelY(Yf) As Long
Static Fy, He, Hp, Ye
Me.ScaleMode = vbPixels
He = Picture1.ScaleHeight
Ye = Picture1.ScaleTop
Hp = Picture1.Height
Fy = Hp / He

ToPixelY = (-Fy * (-Yf + Ye))
ToPixelY = Picture1.ScaleY(Yf - Ye, 0, 3)


End Function

 Sub Vector()
    ReDim Velocidad(L1, M1), uN(L1, M1), vN(L1, M1)
    For j = 1 To M1
        uN(1, j) = u(2, j): uN(L1, j) = u(L1, j)
        vN(1, j) = v(1, j): vN(L1, j) = v(L1, j)
    Next j
    For i = 1 To L1
        vN(i, 1) = v(i, 2): vN(i, M1) = v(i, M1)
        uN(i, 1) = u(i, 1): uN(i, M1) = u(i, M1)
    Next i
    
    For j = 2 To M2
        For i = 2 To L2
        uN(i, j) = (u(i, j) + u(i + 1, j)) / 2
        vN(i, j) = (v(i, j) + v(i, j + 1)) / 2
        Next i
    Next j
    ReDim MV(L1, M1)
    For j = 1 To M1
        For i = 1 To L1
            With Velocidad(i, j)
            .Xb = x(i)
            .Yb = y(j)
            .Xc = .Xb + uN(i, j) * Val(Text2)
            .Yc = .Yb + vN(i, j) * Val(Text2)
            MV(i, j) = Sqr(uN(i, j) ^ 2 + vN(i, j) ^ 2)
            End With
        Next i
    Next j
End Sub



Private Sub Check1_Click()
Call Bo
Picture2.Cls
If Check1 = vbChecked Then
    A(0, 0) = True
Else
    A(0, 0) = False
End If
End Sub

Private Sub Check2_Click()
Call Bo
Picture2.Cls
If Check2 = vbChecked Then
    A(1, 0) = True
    A(1, 1) = Val(Text3)
    A(1, 2) = Val(Text4)
    A(1, 3) = Val(Text5)
    Check5 = 0
    Check6 = 0
    
    Check5.Enabled = True
    Check6.Enabled = True
Else
    A(1, 0) = False
    A(1, 1) = 0
    A(1, 2) = 0
    A(1, 3) = 0
    Check5 = 0
    Check6 = 0
    Check5.Enabled = False
    Check6.Enabled = False
End If

End Sub


Private Sub Check3_Click()
Call Bo
Picture2.Cls
If Check3 = vbChecked Then
    A(2, 0) = True
    A(2, 1) = Val(Text1)
Else
    A(2, 0) = False
    A(2, 1) = 0
End If

End Sub


Private Sub Check4_Click()
Call Bo
Picture2.Cls
If Check4 = vbChecked Then
    A(3, 0) = True
    A(3, 1) = Val(Text2)
Else
    A(3, 0) = False
    A(3, 1) = 0
End If

End Sub


Private Sub Check5_Click()
Dim cMax As Double
If Check5.Value = vbChecked Then
    cMax = FMax(T)
    Text3 = Format(cMax, "0.00")
    Text3.Enabled = False
Else
    Text3.Enabled = True
    Text3 = ""
End If
End Sub

Private Sub Check6_Click()
Dim cMin As Double
If Check6.Value = vbChecked Then
    cMin = FMin(T)
    Text4 = Format(cMin, "0.00")
    Text4.Enabled = False
Else
    Text4.Enabled = True
    Text4 = ""
End If
    
End Sub

Private Sub Command1_Click()
    Fespere.Show
    DoEvents
    Picture1.FillStyle = 0
    Dim i As Single, j As Single, Po() As POINTAPI, n As Long, p1(1 To 3) As POINTAPI, p2(1 To 3) As POINTAPI
    Picture1.DrawStyle = 0
    Call Bo
    Dim cMax As Double, cMin As Double, Difff As Double
    Picture2.Cls
    If A(0, 0) Then 'Gradiente de temperatura
        Picture2.Font.Name = "Times New Roman"
        Picture2.BackColor = vbWhite
        Picture2.ForeColor = vbBlack
        Picture2.Font.Size = 7
        Picture2.Font.Size = 4 * Picture2.Font.Size
        Picture2.CurrentX = 35
        Picture2.CurrentY = 10
        Picture2.Print "T"
        Picture2.Font.Size = Picture2.Font.Size / 4
        Picture1.DrawWidth = 1
        
        cMax = FMax(T)
        cMin = FMin(T)
        Picture1.DrawStyle = 0
        c = 255
        Coc = cMax
        If cMax = 0 Or cMin = 0 Then
            Ll = cMax - cMin > 0.0001
        ElseIf Fix(Log10(Abs(cMax))) = 0 Or Fix(Log10(Abs(cMin))) = 0 Then
            Ll = cMax - cMin > 0.0001
        Else
            Ll = ((cMax / Int(Log10(Abs(cMax))) - cMin / Int(Log10(Abs(cMin)))) > 0.0001)
        End If
        st = IIf(Ll, 16, 256)
        Nst = IIf(Ll, (cMax - cMin) / 256 * st, 0)
        For CC = 0 To 255 Step st
            c = CC
            If CC >= 255 And (Ll) Then CC = 255: Coc = cMin
            For j = 1 To M1 - 1
                For i = 1 To L1 - 1
                    n = 0
                    If Coc >= T(i, j) Then
                        n = n + 1
                        ReDim Preserve Po(1 To n) As POINTAPI
                        Po(n).x = ToPixelX(x(i))
                        Po(n).y = ToPixelY(y(j))
                    Else
                        If Coc > T(i, j + 1) And Coc <= T(i, j) Then
                            n = n + 1
                            ReDim Preserve Po(1 To n) As POINTAPI
                            dT = T(i, j) - T(i, j + 1)
                            dy = y(j) - y(j + 1)
                            dT1 = T(i, j) - Coc
                            ddy = dy * dT1 / dT
                            Po(n).x = ToPixelX(x(i))
                            Po(n).y = ToPixelY(y(j) - ddy)
                        End If
                        If Coc > T(i + 1, j) And Coc <= T(i, j) Then
                            n = n + 1
                            ReDim Preserve Po(1 To n) As POINTAPI
                            dT = T(i, j) - T(i + 1, j)
                            dx = x(i) - x(i + 1)
                            dT1 = T(i, j) - Coc
                            ddx = dx * dT1 / dT
                            Po(n).x = ToPixelX(x(i) - ddx)
                            Po(n).y = ToPixelY(y(j))
                        End If
                    End If
                    If Coc >= T(i + 1, j) Then
                        n = n + 1
                        ReDim Preserve Po(1 To n) As POINTAPI
                        Po(n).x = ToPixelX(x(i + 1))
                        Po(n).y = ToPixelY(y(j))
                    Else
                        If Coc > T(i, j) And Coc <= T(i + 1, j) Then
                            n = n + 1
                            ReDim Preserve Po(1 To n) As POINTAPI
                            dT = T(i + 1, j) - T(i, j)
                            dx = x(i + 1) - x(i)
                            dT1 = T(i + 1, j) - Coc
                            ddx = dx * dT1 / dT
                            Po(n).x = ToPixelX(x(i + 1) - ddx)
                            Po(n).y = ToPixelY(y(j))
                        End If
                        If Coc > T(i + 1, j + 1) And Coc <= T(i + 1, j) Then
                            n = n + 1
                            ReDim Preserve Po(1 To n) As POINTAPI
                            dT = T(i + 1, j) - T(i + 1, j + 1)
                            dy = y(j) - y(j + 1)
                            dT1 = T(i + 1, j) - Coc
                            ddy = dy * dT1 / dT
                            Po(n).x = ToPixelX(x(i + 1))
                            Po(n).y = ToPixelY(y(j) - ddy)
                        End If
                    End If
                    
                    If Coc >= T(i + 1, j + 1) Then
                        n = n + 1
                        ReDim Preserve Po(1 To n) As POINTAPI
                        Po(n).x = ToPixelX(x(i + 1))
                        Po(n).y = ToPixelY(y(j + 1))
                    Else
                        If Coc > T(i + 1, j) And Coc <= T(i + 1, j + 1) Then
                            n = n + 1
                            ReDim Preserve Po(1 To n) As POINTAPI
                            dT = T(i + 1, j + 1) - T(i + 1, j)
                            dy = y(j + 1) - y(j)
                            dT1 = T(i + 1, j + 1) - Coc
                            ddy = dy * dT1 / dT
                            Po(n).x = ToPixelX(x(i + 1))
                            Po(n).y = ToPixelY(y(j + 1) - ddy)
                        End If
                        If Coc > T(i, j + 1) And Coc <= T(i + 1, j + 1) Then
                            n = n + 1
                            ReDim Preserve Po(1 To n) As POINTAPI
                            dT = T(i + 1, j + 1) - T(i, j + 1)
                            dx = x(i + 1) - x(i)
                            dT1 = T(i + 1, j + 1) - Coc
                            ddx = dx * dT1 / dT
                            Po(n).x = ToPixelX(x(i + 1) - ddx)
                            Po(n).y = ToPixelY(y(j + 1))
                        End If
                    End If
                    
                    If Coc >= T(i, j + 1) Then
                        n = n + 1
                        ReDim Preserve Po(1 To n) As POINTAPI
                        Po(n).x = ToPixelX(x(i))
                        Po(n).y = ToPixelY(y(j + 1))
                    Else
                        If Coc > T(i + 1, j + 1) And Coc <= T(i, j + 1) Then
                            n = n + 1
                            ReDim Preserve Po(1 To n) As POINTAPI
                            dT = T(i, j + 1) - T(i + 1, j + 1)
                            dx = x(i) - x(i + 1)
                            dT1 = T(i, j + 1) - Coc
                            ddx = dx * dT1 / dT
                            Po(n).x = ToPixelX(x(i) - ddx)
                            Po(n).y = ToPixelY(y(j + 1))
                        End If
                        If Coc > T(i, j) And Coc <= T(i, j + 1) Then
                            n = n + 1
                            ReDim Preserve Po(1 To n) As POINTAPI
                            dT = T(i, j + 1) - T(i, j)
                            dy = y(j + 1) - y(j)
                            dT1 = T(i, j + 1) - Coc
                            ddy = dy * dT1 / dT
                            Po(n).x = ToPixelX(x(i))
                            Po(n).y = ToPixelY(y(j + 1) - ddy)
                        End If
                    End If
                    If n >= 6 Then
                        prom = (T(i, j) + T(i, j + 1) + T(i + 1, j + 1) + T(i + 1, j)) / 4
                        If Coc < prom Then
                            If Coc < T(i, j) Then
                                p1(1) = Po(1)
                                p1(2) = Po(2)
                                p1(3) = Po(6)
                                p2(1) = Po(3)
                                p2(2) = Po(4)
                                p2(3) = Po(5)
                                Call Poly(Picture1, p1(), 3, RGB(255 - CC, 0, CC))
                                Call Poly(Picture1, p2(), 3, RGB(255 - CC, 0, CC))
                            Else
                                p1(1) = Po(1)
                                p1(2) = Po(2)
                                p1(3) = Po(3)
                                p2(1) = Po(4)
                                p2(2) = Po(5)
                                p2(3) = Po(6)
                                Call Poly(Picture1, p1(), 3, RGB(255 - CC, 0, CC))
                                Call Poly(Picture1, p2(), 3, RGB(255 - CC, 0, CC))
                            End If
                        End If
                    End If
                    If n > 2 And n < 6 Then Call Poly(Picture1, Po(), n, RGB(255 - CC, 0, CC))
                
                Next i
             Next j
            Picture1.Refresh
            DoEvents
            Picture2.Line (10, 50 + 20 * CC / 16)-(30, 70 + 20 * CC / 16), RGB(255 - CC, 0, CC), BF
            Picture2.Line (10, 50 + 20 * CC / 16)-(30, 70 + 20 * CC / 16), vbBlack, B
            Picture2.Line (10, 50 + 20 * CC / 16)-(35, 50 + 20 * CC / 16), vbBlack
            Picture2.CurrentY = Picture2.CurrentY - 6
            Picture2.CurrentX = Picture2.CurrentX + 4
            Picture2.Print Format(Coc, "0.00E+00")
            Coc = Coc - Nst
            Coc = IIf(Coc < cMin, cMin, Coc)
        Next CC
        If (Ll) Then
            Picture2.Line (10, 50 + 20 * CC / 16)-(35, 50 + 20 * CC / 16), vbBlack
            Picture2.CurrentY = Picture2.CurrentY - 6
            Picture2.CurrentX = Picture2.CurrentX + 4
            Picture2.Print Format(Coc, "0.00E+00")
            Picture1.DrawStyle = 0
        End If
    End If
    If A(1, 0) Then  'isotermas
        Picture1.DrawWidth = 1
        oldcolor = Picture1.ForeColor
        Picture1.ForeColor = QBColor(13)
        Picture1.DrawStyle = 0
        st = Val(Text5)
        cMax = FMax(T)
        cMin = FMin(T)
        
        If Check5.Value = vbChecked Then
            cMax = FMax(T)
        Else
            cMax = Val(Text3)
        End If
        If Check6.Value = vbChecked Then
            cMin = FMin(T)
        Else
            cMin = Val(Text4)
        End If
        Nst = (cMax - cMin) / (st)
        Coc = cMax
        For CC = 0 To st
        If CC = st Then Coc = cMin
            For j = 1 To M1 - 1
                For i = 1 To L1 - 1
                    Llinea = False
                    
                    If T(i, j) <= Coc Then
                        If Coc < T(i + 1, j) Then
                            dT = T(i, j) - T(i + 1, j)
                            dx = x(i) - x(i + 1)
                            dT1 = T(i, j) - Coc
                            ddx = dx * dT1 / dT
                            Picture1.PSet (x(i) - ddx, y(j))
                            If Coc = cMin Then
                                Picture1.CurrentX = Picture1.CurrentX - Picture1.ScaleX(0.45, 4, 0)
                                Picture1.CurrentY = Picture1.CurrentY - Picture1.ScaleY(0.45, 4, 0)
                                Picture1.Print "x"
                            End If
                            Picture1.PSet (x(i) - ddx, y(j))
                            Llinea = True
                        End If
                        If Coc < T(i, j + 1) Then
                            dT = T(i, j) - T(i, j + 1)
                            dy = y(j) - y(j + 1)
                            dT1 = T(i, j) - Coc
                            ddy = dy * dT1 / dT
                            If Llinea Then Picture1.Line -(x(i), y(j) - ddy)
                            Picture1.PSet (x(i), y(j) - ddy)
                            If Coc = cMin Then
                                Picture1.CurrentX = Picture1.CurrentX - Picture1.ScaleX(0.45, 4, 0)
                                Picture1.CurrentY = Picture1.CurrentY - Picture1.ScaleY(0.45, 4, 0)
                                Picture1.Print "x"
                            End If
                            Picture1.PSet (x(i), y(j) - ddy)
                            Llinea = True
                        End If
                    End If
                    If T(i + 1, j) <= Coc Then
                        If Coc < T(i, j) Then
                            dT = T(i + 1, j) - T(i, j)
                            dx = x(i + 1) - x(i)
                            dT1 = T(i + 1, j) - Coc
                            ddx = dx * dT1 / dT
                             If Llinea Then Picture1.Line -(x(i + 1) - ddx, y(j))
                             Picture1.PSet (x(i + 1) - ddx, y(j))
                             If Coc = cMin Then
                               Picture1.CurrentX = Picture1.CurrentX - Picture1.ScaleX(0.45, 4, 0)
                               Picture1.CurrentY = Picture1.CurrentY - Picture1.ScaleY(0.45, 4, 0)
                               Picture1.Print "x"
                           End If
                            Picture1.PSet (x(i + 1) - ddx, y(j))
                            Llinea = True
                        End If
                        If Coc < T(i + 1, j + 1) Then
                            dT = T(i + 1, j) - T(i + 1, j + 1)
                            dy = y(j) - y(j + 1)
                            dT1 = T(i + 1, j) - Coc
                            ddy = dy * dT1 / dT
                            If Llinea Then Picture1.Line -(x(i + 1), y(j) - ddy)
                            Picture1.PSet (x(i + 1), y(j) - ddy)
                            If Coc = cMin Then
                                Picture1.CurrentX = Picture1.CurrentX - Picture1.ScaleX(0.45, 4, 0)
                                Picture1.CurrentY = Picture1.CurrentY - Picture1.ScaleY(0.45, 4, 0)
                                Picture1.Print "x"
                            End If
                            Picture1.PSet (x(i + 1), y(j) - ddy)
                            Llinea = True
                        End If
                    End If
                    If T(i, j + 1) <= Coc Then
                        If Coc < T(i + 1, j + 1) Then
                            dT = T(i, j + 1) - T(i + 1, j + 1)
                            dx = x(i) - x(i + 1)
                            dT1 = T(i, j + 1) - Coc
                            ddx = dx * dT1 / dT
                            If Llinea Then Picture1.Line -(x(i) - ddx, y(j + 1))
                            Picture1.PSet (x(i) - ddx, y(j + 1))
                            If Coc = cMin Then
                                Picture1.CurrentX = Picture1.CurrentX - Picture1.ScaleX(0.45, 4, 0)
                                Picture1.CurrentY = Picture1.CurrentY - Picture1.ScaleY(0.45, 4, 0)
                                Picture1.Print "x"
                            End If
                            Picture1.PSet (x(i) - ddx, y(j + 1))
                            Llinea = True
                        End If
                        If Coc < T(i, j) Then
                            dT = T(i, j + 1) - T(i, j)
                            dy = y(j + 1) - y(j)
                            dT1 = T(i, j + 1) - Coc
                            ddy = dy * dT1 / dT
                            If Llinea Then Picture1.Line -(x(i), y(j + 1) - ddy)
                            Picture1.PSet (x(i), y(j + 1) - ddy)
                            If Coc = cMin Then
                                Picture1.CurrentX = Picture1.CurrentX - Picture1.ScaleX(0.45, 4, 0)
                                Picture1.CurrentY = Picture1.CurrentY - Picture1.ScaleY(0.45, 4, 0)
                                Picture1.Print "x"
                            End If
                            Picture1.PSet (x(i), y(j + 1) - ddy)
                            Llinea = True
                        End If
                    End If
                    If T(i + 1, j + 1) <= Coc Then
                        If Coc < T(i, j + 1) Then
                            dT = T(1 + i, j + 1) - T(i, j + 1)
                            dx = x(i + 1) - x(i)
                            dT1 = T(i + 1, j + 1) - Coc
                            ddx = dx * dT1 / dT
                            If Llinea Then Picture1.Line -(x(i + 1) - ddx, y(j + 1))
                            Picture1.PSet (x(i + 1) - ddx, y(j + 1))
                            If Coc = cMin Then
                                Picture1.CurrentX = Picture1.CurrentX - Picture1.ScaleX(0.45, 4, 0)
                                Picture1.CurrentY = Picture1.CurrentY - Picture1.ScaleY(0.45, 4, 0)
                                Picture1.Print "x"
                            End If
                            Picture1.PSet (x(i + 1) - ddx, y(j + 1))
                            Llinea = True
                        End If
                        If Coc < T(i + 1, j) Then
                            dT = T(i + 1, j + 1) - T(i + 1, j)
                            dy = y(j + 1) - y(j)
                            dT1 = T(i + 1, j + 1) - Coc
                            ddy = dy * dT1 / dT
                            If Llinea Then Picture1.Line -(x(i + 1), y(j + 1) - ddy)
                            Picture1.PSet (x(i + 1), y(j + 1) - ddy)
                            If Coc = cMin Then
                                Picture1.CurrentX = Picture1.CurrentX - Picture1.ScaleX(0.45, 4, 0)
                                Picture1.CurrentY = Picture1.CurrentY - Picture1.ScaleY(0.45, 4, 0)
                                Picture1.Print "x"
                            End If
                            Picture1.PSet (x(i + 1), y(j + 1) - ddy)
                            Llinea = True
                        End If
                    End If
                Next i
            Next j
            'Picture1.Refresh
            'DoEvents
            Coc = Coc - Nst
        Next CC
        Coc = cMax
        For j = 1 To M1 - 1
            For i = 1 To L1 - 1
                Llinea = False
                If T(i, j) >= Coc Then
                    If Coc > T(i + 1, j) Then
                        dT = T(i, j) - T(i + 1, j)
                        dx = x(i) - x(i + 1)
                        dT1 = T(i, j) - Coc
                        ddx = dx * dT1 / dT
                        Picture1.PSet (x(i) - ddx, y(j))
                        Picture1.CurrentX = Picture1.CurrentX - Picture1.ScaleX(0.45, 4, 0)
                        Picture1.CurrentY = Picture1.CurrentY - Picture1.ScaleY(0.45, 4, 0)
                        Picture1.Print "+"
                        Picture1.PSet (x(i) - ddx, y(j))
                        Llinea = True
                    End If
                    If Coc > T(i, j + 1) Then
                        dT = T(i, j) - T(i, j + 1)
                        dy = y(j) - y(j + 1)
                        dT1 = T(i, j) - Coc
                        ddy = dy * dT1 / dT
                        If Llinea Then Picture1.Line -(x(i), y(j) - ddy)
                        Picture1.PSet (x(i), y(j) - ddy)
                        Picture1.CurrentX = Picture1.CurrentX - Picture1.ScaleX(0.45, 4, 0)
                        Picture1.CurrentY = Picture1.CurrentY - Picture1.ScaleY(0.45, 4, 0)
                        Picture1.Print "+"
                        Picture1.PSet (x(i), y(j) - ddy)
                        Llinea = True
                    End If
                End If
                If T(i + 1, j) >= Coc Then
                    If Coc > T(i, j) Then
                        dT = T(i + 1, j) - T(i, j)
                        dx = x(i + 1) - x(i)
                        dT1 = T(i + 1, j) - Coc
                        ddx = dx * dT1 / dT
                         If Llinea Then Picture1.Line -(x(i + 1) - ddx, y(j))
                         Picture1.PSet (x(i + 1) - ddx, y(j))
                         Picture1.CurrentX = Picture1.CurrentX - Picture1.ScaleX(0.45, 4, 0)
                         Picture1.CurrentY = Picture1.CurrentY - Picture1.ScaleY(0.45, 4, 0)
                         Picture1.Print "+"
                         Picture1.PSet (x(i + 1) - ddx, y(j))
                         Llinea = True
                    End If
                    If Coc > T(i + 1, j + 1) Then
                        dT = T(i + 1, j) - T(i + 1, j + 1)
                        dy = y(j) - y(j + 1)
                        dT1 = T(i + 1, j) - Coc
                        ddy = dy * dT1 / dT
                        If Llinea Then Picture1.Line -(x(i + 1), y(j) - ddy)
                        Picture1.PSet (x(i + 1), y(j) - ddy)
                        Picture1.CurrentX = Picture1.CurrentX - Picture1.ScaleX(0.45, 4, 0)
                        Picture1.CurrentY = Picture1.CurrentY - Picture1.ScaleY(0.45, 4, 0)
                        Picture1.Print "+"
                        Picture1.PSet (x(i + 1), y(j) - ddy)
                        Llinea = True
                    End If
                End If
                If T(i, j + 1) >= Coc Then
                    If Coc > T(i + 1, j + 1) Then
                        dT = T(i, j + 1) - T(i + 1, j + 1)
                        dx = x(i) - x(i + 1)
                        dT1 = T(i, j + 1) - Coc
                        ddx = dx * dT1 / dT
                        If Llinea Then Picture1.Line -(x(i) - ddx, y(j + 1))
                        Picture1.PSet (x(i) - ddx, y(j + 1))
                        Picture1.CurrentX = Picture1.CurrentX - Picture1.ScaleX(0.45, 4, 0)
                        Picture1.CurrentY = Picture1.CurrentY - Picture1.ScaleY(0.45, 4, 0)
                        Picture1.Print "+"
                        Picture1.PSet (x(i) - ddx, y(j + 1))
                        Llinea = True
                    End If
                    If Coc > T(i, j) Then
                        dT = T(i, j + 1) - T(i, j)
                        dy = y(j + 1) - y(j)
                        dT1 = T(i, j + 1) - Coc
                        ddy = dy * dT1 / dT
                        If Llinea Then Picture1.Line -(x(i), y(j + 1) - ddy)
                        Picture1.PSet (x(i), y(j + 1) - ddy)
                        Picture1.CurrentX = Picture1.CurrentX - Picture1.ScaleX(0.45, 4, 0)
                        Picture1.CurrentY = Picture1.CurrentY - Picture1.ScaleY(0.45, 4, 0)
                        Picture1.Print "+"
                        Picture1.PSet (x(i), y(j + 1) - ddy)
                        Llinea = True
                    End If
                End If
                If T(i + 1, j + 1) >= Coc Then
                    If Coc > T(i, j + 1) Then
                        dT = T(1 + i, j + 1) - T(i, j + 1)
                        dx = x(i + 1) - x(i)
                        dT1 = T(i + 1, j + 1) - Coc
                        ddx = dx * dT1 / dT
                        If Llinea Then Picture1.Line -(x(i + 1) - ddx, y(j + 1))
                        Picture1.PSet (x(i + 1) - ddx, y(j + 1))
                        Picture1.CurrentX = Picture1.CurrentX - Picture1.ScaleX(0.45, 4, 0)
                        Picture1.CurrentY = Picture1.CurrentY - Picture1.ScaleY(0.45, 4, 0)
                        Picture1.Print "+"
                        Picture1.PSet (x(i + 1) - ddx, y(j + 1))
                        Llinea = True
                    End If
                    If Coc > T(i + 1, j) Then
                        dT = T(i + 1, j + 1) - T(i + 1, j)
                        dy = y(j + 1) - y(j)
                        dT1 = T(i + 1, j + 1) - Coc
                        ddy = dy * dT1 / dT
                        If Llinea Then Picture1.Line -(x(i + 1), y(j + 1) - ddy)
                        Picture1.PSet (x(i + 1), y(j + 1) - ddy)
                        Picture1.CurrentX = Picture1.CurrentX - Picture1.ScaleX(0.45, 4, 0)
                        Picture1.CurrentY = Picture1.CurrentY - Picture1.ScaleY(0.45, 4, 0)
                        Picture1.Print "+"
                        Picture1.PSet (x(i + 1), y(j + 1) - ddy)
                        Llinea = True
                    End If
                End If
            Next i
        Next j
        'Picture1.Refresh
        'DoEvents
        Picture1.ForeColor = oldcolor
10    End If
    
    If A(2, 0) Then   'Lineas de corrientes
        Picture1.DrawWidth = 1
        oldcolor = Picture1.ForeColor
        Picture1.ForeColor = QBColor(2)
        st = Val(Text1) + 1
        cMax = Val(Format(FMax(Pc), "#.00e+00"))
        cMin = Val(Format(FMin(Pc), "#.00e+00"))
        Nst = (cMax - cMin) / st
        Coc = cMin
        For CC = 0 To st
        If CC >= st Then Coc = 0
        'For Coc = cMin To cMax Step Nst
           ' c = c - Fc
            For j = 2 To M1 - 1
                For i = 2 To L1 - 1
                    Llinea = False
                    If Pc(i, j) >= Coc Then
                        If Coc > Pc(i + 1, j) Then
                            dT = Pc(i, j) - Pc(i + 1, j)
                            dx = Xu(i) - Xu(i + 1)
                            dT1 = Pc(i, j) - Coc
                            ddx = dx * dT1 / dT
                            Picture1.PSet (Xu(i) - ddx, Yv(j))
                            Llinea = True
                        End If
                        If Coc > Pc(i, j + 1) Then
                            dT = Pc(i, j) - Pc(i, j + 1)
                            dy = Yv(j) - Yv(j + 1)
                            dT1 = Pc(i, j) - Coc
                            ddy = dy * dT1 / dT
                            If Llinea Then Picture1.Line -(Xu(i), Yv(j) - ddy)
                            Picture1.PSet (Xu(i), Yv(j) - ddy)
                            Llinea = True
                        End If
                    End If
                    If Pc(i + 1, j) >= Coc Then
                        If Coc > Pc(i, j) Then
                            dT = Pc(i + 1, j) - Pc(i, j)
                            dx = Xu(i + 1) - Xu(i)
                            dT1 = Pc(i + 1, j) - Coc
                            ddx = dx * dT1 / dT
                            If Llinea Then Picture1.Line -(Xu(i + 1) - ddx, Yv(j))
                            Picture1.PSet (Xu(i + 1) - ddx, Yv(j))
                            Llinea = True
                        End If
                        If Coc > Pc(i + 1, j + 1) Then
                            dT = Pc(i + 1, j) - Pc(i + 1, j + 1)
                            dy = Yv(j) - Yv(j + 1)
                            dT1 = Pc(i + 1, j) - Coc
                            ddy = dy * dT1 / dT
                            If Llinea Then Picture1.Line -(Xu(i + 1), Yv(j) - ddy)
                            Picture1.PSet (Xu(i + 1), Yv(j) - ddy)
                            Llinea = True
                        End If
                    End If
                    If Pc(i, j + 1) >= Coc Then
                        If Coc > Pc(i + 1, j + 1) Then
                            dT = Pc(i, j + 1) - Pc(i + 1, j + 1)
                            dx = Xu(i) - Xu(i + 1)
                            dT1 = Pc(i, j + 1) - Coc
                            ddx = dx * dT1 / dT
                            If Llinea Then Picture1.Line -(Xu(i) - ddx, Yv(j + 1))
                            Picture1.PSet (Xu(i) - ddx, Yv(j + 1))
                            Llinea = True
                        End If
                        If Coc > Pc(i, j) Then
                            dT = Pc(i, j + 1) - Pc(i, j)
                            dy = Yv(j + 1) - Yv(j)
                            dT1 = Pc(i, j + 1) - Coc
                            ddy = dy * dT1 / dT
                            If Llinea Then Picture1.Line -(Xu(i), Yv(j + 1) - ddy)
                            Picture1.PSet (Xu(i), Yv(j + 1) - ddy)
                            Llinea = True
                        End If
                    End If
                    If Pc(i + 1, j + 1) >= Coc Then
                        If Coc > Pc(i, j + 1) Then
                            dT = Pc(1 + i, j + 1) - Pc(i, j + 1)
                            dx = Xu(i + 1) - Xu(i)
                            dT1 = Pc(i + 1, j + 1) - Coc
                            ddx = dx * dT1 / dT
                            If Llinea Then Picture1.Line -(Xu(i + 1) - ddx, Yv(j + 1))
                            Picture1.PSet (Xu(i + 1) - ddx, Yv(j + 1))
                            Llinea = True
                        End If
                        If Coc > Pc(i + 1, j) Then
                            dT = Pc(i + 1, j + 1) - Pc(i + 1, j)
                            dy = Yv(j + 1) - Yv(j)
                            dT1 = Pc(i + 1, j + 1) - Coc
                            ddy = dy * dT1 / dT
                            If Llinea Then Picture1.Line -(Xu(i + 1), Yv(j + 1) - ddy)
                            Picture1.PSet (Xu(i + 1), Yv(j + 1) - ddy)
                            Llinea = True
                        End If
                    End If
                Next i
             Next j
             Coc = Coc + Nst
        Next CC
    Picture1.ForeColor = oldcolor
    End If
    If A(3, 0) Then 'Vectores
        Call Vector
        oldcolor = Picture1.ForeColor
        Picture1.ForeColor = vbBlack
        Picture1.DrawWidth = 1
        For j = 1 To M1
            For i = 1 To L1
                With Velocidad(i, j)
                    If Ns = 0 Then
                        Picture1.Line (.Xb, .Yb)-(.Xc, .Yc)
                        Picture1.DrawWidth = 2
                        Picture1.PSet (.Xc, .Yc)
                        Picture1.DrawWidth = 1
                    Else
                        For K = 1 To Ns
                            If (S(0, K) <= .Xb And S(2, K) >= .Xb) And (S(1, K) <= .Yb And S(3, K) >= .Yb) Then
                            
                            Else
                                Picture1.Line (.Xb, .Yb)-(.Xc, .Yc)
                                Picture1.DrawWidth = 2
                                Picture1.PSet (.Xc, .Yc)
                                Picture1.DrawWidth = 1
                            End If
                        Next K
                    End If
                End With
                'Picture1.Refresh

            Next i
        Next j
        Picture1.ForeColor = oldcolor
    End If
    Picture1.FillStyle = 1
    For i = 1 To Ns
        Picture1.Line (S(0, i), S(1, i))-(S(2, i), S(3, i)), QBColor(14), B
    Next i
    Fespere.Hide
End Sub

Private Sub Command2_Click()
    End
End Sub


Private Sub Command3_Click()
    id = Shell("igs.exe", vbNormalFocus)
    End
End Sub



Private Sub Command4_Click()

End Sub

Private Sub Command5_Click()
    Planos.Show
    Me.Hide
End Sub

Private Sub Form_Load()
    Ll = True
    Call CargaA
    Xl = x(L1)
    Yl = y(M1)
    EscalaXY = Xl
    If Yl > Xl Then
        EscalaXY = Yl
    End If
    Mediaescala = (EscalaXY * 1.1) / 2
    With Picture1
        .ScaleHeight = -2 * Mediaescala
        .ScaleWidth = 2 * Mediaescala
        .ScaleLeft = -Mediaescala + Xl / 2
        .ScaleTop = Yl / 2 + Mediaescala
    End With
    Call Vector
End Sub


Private Sub Form_Unload(Cancel As Integer)
End
End Sub


Private Sub Picture1_DblClick()
Arch = InputBox("Nombre del archivo de imajen bmp", "Guardar imajen")
If Arch <> "" Then
    SavePicture Picture1.Image, Arch & ".bmp"
    If Check1.Value = vbChecked Then
        SavePicture Picture2.Image, Arch & "Gra.bmp"
    End If
End If
End Sub


