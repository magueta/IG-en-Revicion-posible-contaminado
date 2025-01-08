VERSION 5.00
Begin VB.Form Planos 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Planos"
   ClientHeight    =   5985
   ClientLeft      =   90
   ClientTop       =   465
   ClientWidth     =   9465
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   ScaleHeight     =   5985
   ScaleWidth      =   9465
   Begin VB.CommandButton Command4 
      Caption         =   "Data"
      Height          =   375
      Left            =   1140
      TabIndex        =   14
      Top             =   5400
      Width           =   975
   End
   Begin VB.PictureBox Picture2 
      Appearance      =   0  'Flat
      BackColor       =   &H00000000&
      ForeColor       =   &H80000008&
      Height          =   1935
      Left            =   120
      ScaleHeight     =   1905
      ScaleWidth      =   1905
      TabIndex        =   13
      Top             =   3300
      Width           =   1935
   End
   Begin VB.PictureBox Picture1 
      Appearance      =   0  'Flat
      AutoRedraw      =   -1  'True
      BackColor       =   &H80000005&
      ForeColor       =   &H80000008&
      Height          =   5715
      Left            =   2280
      ScaleHeight     =   379
      ScaleMode       =   3  'Píxel
      ScaleWidth      =   467
      TabIndex        =   12
      Top             =   120
      Width           =   7035
   End
   Begin VB.TextBox Text2 
      Height          =   255
      Left            =   1200
      TabIndex        =   10
      Top             =   1260
      Width           =   495
   End
   Begin VB.Frame Frame1 
      Height          =   1335
      Left            =   120
      TabIndex        =   6
      Top             =   1860
      Width           =   1935
      Begin VB.OptionButton Option1 
         Caption         =   "| Velocidad |"
         Height          =   195
         Index           =   2
         Left            =   60
         TabIndex        =   9
         Top             =   1020
         Width           =   1695
      End
      Begin VB.OptionButton Option1 
         Caption         =   "Presión"
         Height          =   195
         Index           =   1
         Left            =   60
         TabIndex        =   8
         Top             =   660
         Width           =   1695
      End
      Begin VB.OptionButton Option1 
         Caption         =   "Temperatura"
         Height          =   195
         Index           =   0
         Left            =   60
         TabIndex        =   7
         Top             =   300
         Value           =   -1  'True
         Width           =   1695
      End
   End
   Begin VB.OptionButton Option2 
      Caption         =   "Horizontal"
      Height          =   195
      Index           =   1
      Left            =   360
      TabIndex        =   5
      Top             =   480
      Width           =   1275
   End
   Begin VB.OptionButton Option2 
      Caption         =   "Vertical"
      Height          =   195
      Index           =   0
      Left            =   360
      TabIndex        =   4
      Top             =   180
      Value           =   -1  'True
      Width           =   1275
   End
   Begin VB.HScrollBar HScroll1 
      Height          =   195
      Left            =   120
      Max             =   50
      Min             =   1
      TabIndex        =   2
      Top             =   1560
      Value           =   1
      Width           =   1995
   End
   Begin VB.TextBox Text1 
      Height          =   255
      Left            =   1200
      TabIndex        =   1
      Top             =   960
      Width           =   495
   End
   Begin VB.CommandButton Command1 
      Caption         =   "Ocultar"
      Height          =   375
      Left            =   60
      TabIndex        =   0
      Top             =   5400
      Width           =   915
   End
   Begin VB.Label Label2 
      Caption         =   "Label1"
      Height          =   195
      Left            =   480
      TabIndex        =   11
      Top             =   1260
      Width           =   615
   End
   Begin VB.Label Label1 
      Caption         =   "Label1"
      Height          =   195
      Left            =   480
      TabIndex        =   3
      Top             =   960
      Width           =   615
   End
End
Attribute VB_Name = "Planos"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False



Sub grafica(vg, d, iff, mm, vd)
    ReDim Graf(1, mm)
    Nst = 5
    Picture1.Cls
    Picture1.Line (40, 10)-(Picture1.ScaleWidth - 10, Picture1.ScaleHeight - 40), vbBlack, B
    kkk = iff
    Mavg = mma(vg, d, kkk, mm)
    Mivg = mmi(vg, d, kkk, mm)
    Escalay = (Picture1.ScaleHeight - 50) / (vd(mm))
    
   If Mavg = 0 Or Mivg = 0 Then
    If Mavg - Mivg < 0.1 Then
            If Mavg = 0 And Mivg = 0 Then
                Mavg = 0.05
                Mivg = -0.05
                Escalax = (Picture1.ScaleWidth - 50) / (Mavg - Mivg)
            Else
                Mavg = mma(vg, d, kkk, mm) + 0.05 * mma(vg, d, kkk, mm)
                Mivg = mmi(vg, d, kkk, mm) - 0.05 * mma(vg, d, kkk, mm)
                Escalax = (Picture1.ScaleWidth - 50) / (Mavg - Mivg)
            End If
        Else
            ErE = (Mavg - Mivg) / (Nst - 2)
            Mavg = mma(vg, d, kkk, mm) + ErE
            Mivg = mmi(vg, d, kkk, mm) - ErE
            Escalax = (Picture1.ScaleWidth - 50) / (Mavg - Mivg)
        End If
   Else
        If Abs(Mavg / 10 ^ Fix(Log10(Abs(Mavg))) - Mivg / 10 ^ Fix(Log10(Abs(Mivg)))) < 0.1 Then
            Mavg = mma(vg, d, kkk, mm) + 0.05 * Abs(mma(vg, d, kkk, mm))
            Mivg = mmi(vg, d, kkk, mm) - 0.05 * Abs(mma(vg, d, kkk, mm))
            Escalax = (Picture1.ScaleWidth - 50) / (Mavg - Mivg)
        Else
            ErE = (Mavg - Mivg) / (Nst - 2)
            Mavg = mma(vg, d, kkk, mm) + ErE
            Mivg = mmi(vg, d, kkk, mm) - ErE
            Escalax = (Picture1.ScaleWidth - 50) / (Mavg - Mivg)
        End If
    End If
    For i = 1 To mm
        Graf(1, i) = Picture1.ScaleHeight - 40 - vd(i) * Escalay ' vd(i) * escalay + 40
        If d = "y" Then
            Graf(0, i) = (vg(kkk, i) - Mivg) * Escalax + 40 'Picture1.ScaleHeight - 40 - (vg(kkk, i) - Mivg) * escalay
            Text2 = x(iff)
        Else
            Graf(0, i) = (vg(i, kkk) - Mivg) * Escalax + 40 'Picture1.ScaleHeight - 40 - (vg(i, kkk) - Mivg) * escalay
            Text2 = y(iff)
        End If
    Next i
        Picture1.PSet (Graf(0, 1), Graf(1, 1))
        
    For j = 2 To mm
        Picture1.Line -(Graf(0, j), Graf(1, j)), vbRed
    Next j
    Text1 = iff
    ErE = vd(mm) / Nst
    'ErE = (Mavg - Mivg) / 5
    Picture1.Font.Name = "Times New Roman"
    Picture1.Font.Size = 7
    Picture1.Font.Bold = False
    
    For i = 0 To Nst
        Picture1.Line (45, Picture1.ScaleHeight - 40 - (i * ErE) _
        * Escalay)-(35, Picture1.ScaleHeight - 40 - (i * ErE) _
        * Escalay)
        Picture1.CurrentX = 3
        Picture1.CurrentY = Picture1.CurrentY - 7
        Picture1.Print Format(i * ErE, "0.00e+##")
        'Picture1.Print Format(Mivg + i * ErE, "0.00e+##")
    Next i
    'ErE = vd(mm) / 5
    ErE = (Mavg - Mivg) / Nst
    For i = 0 To Nst
        Picture1.Line (i * ErE * Escalax + 40, Picture1.ScaleHeight - 45)- _
        (i * ErE * Escalax + 40, Picture1.ScaleHeight - 35)
        Picture1.CurrentX = Picture1.CurrentX - 25
        Picture1.CurrentY = Picture1.CurrentY + 3
        Picture1.Print Format((Mivg + i * ErE), "0.000e+##")
        'Picture1.Print Format(i * ErE, "0.00e+##")
    Next i
    
    Picture2.Cls
    For i = 1 To L1
        For j = 1 To M1
            Picture2.PSet (x(i), y(j)), QBColor(2)
        Next j
    Next i
    Picture2.FillStyle = 1
    For i = 1 To Ns
        Picture2.Line (S(0, i), S(1, i))-(S(2, i), S(3, i)), vbYellow, B
    Next i
    Picture2.Line (0, 0)-(x(L1), y(M1)), vbWhite, B
    Select Case d
        Case "y"
            Picture2.Line (x(iff), 0)-(x(iff), y(M1)), vbRed
        Case Else
            Picture2.Line (0, y(iff))-(x(L1), y(iff)), vbRed
    End Select
End Sub


Private Sub Command1_Click()
    Post.Show
    Me.Hide
End Sub

Private Sub Command4_Click()
Me.Hide
Data.Show
End Sub

Private Sub Form_Load()
Label1.Caption = "i"
Label2.Caption = "X(i)"
Call grafica(T, "y", 1, M1, y)
HScroll1.Min = 1
HScroll1.Max = L1
HScroll1.SmallChange = 1
HScroll1.LargeChange = 1


Xl = x(L1)
Yl = y(M1)
EscalaXY = Xl
If Yl > Xl Then
    EscalaXY = Yl
End If
Mediaescala = (EscalaXY * 1.1) / 2
Picture2.AutoRedraw = True
With Picture2
    .ScaleHeight = -2 * Mediaescala
    .ScaleWidth = 2 * Mediaescala
    .ScaleLeft = -Mediaescala + Xl / 2
    .ScaleTop = Yl / 2 + Mediaescala
End With
    Picture2.DrawWidth = 1
    For i = 1 To L1
        For j = 1 To M1
            Picture2.PSet (x(i), y(j)), QBColor(2)
        Next j
    Next i
    Picture1.FillStyle = 1
    For i = 1 To Ns
        Picture2.Line (S(0, i), S(1, i))-(S(2, i), S(3, i)), vbYellow, B
    Next i
    Picture2.Line (0, 0)-(x(L1), y(M1)), vbWhite, B
    Picture2.Line (0, 0)-(0, y(M1)), vbRed
End Sub
 Function mma(aa, tiff, iff, mim)
    Select Case tiff
        Case "y", "Y"
            mmaux = aa(iff, 1)
            For j = 2 To mim
                If mmaux < aa(iff, j) Then mmaux = aa(iff, j)
            Next j
        Case "x", "X"
            mmaux = aa(1, iff)
            For i = 2 To mim
                If mmaux < aa(i, iff) Then mmaux = aa(i, iff)
            Next i
    End Select
    mma = mmaux

End Function

 Function mmi(aa, tiff, iff, mim)
    Select Case tiff
        Case "Y", "y"
            mmaux = aa(iff, 1)
            For j = 2 To mim
                If mmaux > aa(iff, j) Then mmaux = aa(iff, j)
            Next j
        Case "x", "X"
            mmaux = aa(1, iff)
            For i = 2 To mim
                If mmaux > aa(i, iff) Then mmaux = aa(i, iff)
            Next i
    End Select
    mmi = mmaux

End Function

Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)
End
End Sub

Private Sub HScroll1_Change()
    Select Case Option2(0).Value
        Case True
            If Option1(0) = True Then
                Call grafica(T, "y", HScroll1.Value, M1, y)
            ElseIf Option1(1) = True Then
                Call grafica(P, "y", HScroll1.Value, M1, y)
            ElseIf Option1(2) = True Then
                Call grafica(MV, "y", HScroll1.Value, M1, y)
            End If
        Case Else
            If Option1(0) = True Then
                Call grafica(T, "x", HScroll1.Value, L1, x)
            ElseIf Option1(1) = True Then
                Call grafica(P, "x", HScroll1.Value, L1, x)
            ElseIf Option1(2) = True Then
                Call grafica(MV, "x", HScroll1.Value, L1, x)
            End If
    End Select
End Sub

Private Sub HScroll1_Scroll()
    Select Case Option2(0).Value
        Case True
            If Option1(0) = True Then
                Call grafica(T, "y", HScroll1.Value, M1, y)
            ElseIf Option1(1) = True Then
                Call grafica(P, "y", HScroll1.Value, M1, y)
            ElseIf Option1(2) = True Then
                Call grafica(MV, "y", HScroll1.Value, M1, y)
            End If
        Case Else
            If Option1(0) = True Then
                Call grafica(T, "x", HScroll1.Value, L1, x)
            ElseIf Option1(1) = True Then
                Call grafica(P, "x", HScroll1.Value, L1, x)
            ElseIf Option1(2) = True Then
                Call grafica(MV, "x", HScroll1.Value, L1, x)
            End If
    End Select
End Sub


Private Sub Option1_Click(Index As Integer)
HScroll1.Value = 1
Select Case Index
    Case 0
        If Option2(1) Then
            Call grafica(T, "x", HScroll1.Value, L1, x)
        Else
            Call grafica(T, "y", HScroll1.Value, M1, y)
        End If
    Case 1
        If Option2(1) Then
            Call grafica(P, "x", HScroll1.Value, L1, x)
        Else
            Call grafica(P, "y", HScroll1.Value, M1, y)
        End If
    Case 2
        
        If Option2(1) Then
            Call grafica(MV, "x", HScroll1.Value, L1, x)
        Else
            Call grafica(MV, "y", HScroll1.Value, M1, y)
        End If
End Select
End Sub

Private Sub Option2_Click(Index As Integer)
HScroll1.Value = 1
Select Case Index
    Case 0
        Label1.Caption = "i"
        Label2.Caption = "X(i)"
        If Option1(0) Then
            Call grafica(T, "y", HScroll1.Value, M1, y)
        ElseIf Option1(1) Then
            Call grafica(P, "y", HScroll1.Value, M1, y)
        ElseIf Option1(2) Then
            Call grafica(MV, "y", HScroll1.Value, M1, y)
        End If
        HScroll1.Min = 1
        HScroll1.Max = L1
        HScroll1.SmallChange = 1
        HScroll1.LargeChange = 1

    Case 1
        Label1.Caption = "j"
        Label2.Caption = "Y(j)"
            If Option1(0) Then
                Call grafica(T, "x", HScroll1.Value, L1, x)
            ElseIf Option1(1) Then
                Call grafica(P, "x", HScroll1.Value, L1, x)
            ElseIf Option1(2) Then
                Call grafica(MV, "x", HScroll1.Value, L1, x)
            End If
        HScroll1.Min = 1
        HScroll1.Max = M1
        HScroll1.SmallChange = 1
        HScroll1.LargeChange = 1

End Select
End Sub


Private Sub Picture1_Click()
Arch = InputBox("Nombre del archivo de imajen bmp", "Guardar imajen")
If Arch <> "" Then
    SavePicture Picture1.Image, Arch & ".bmp"
    SavePicture Picture2.Image, Arch & "pla.bmp"
    
End If

End Sub

