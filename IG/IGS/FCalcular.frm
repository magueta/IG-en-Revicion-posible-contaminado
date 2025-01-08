VERSION 5.00
Begin VB.Form Form1 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Resolver"
   ClientHeight    =   6450
   ClientLeft      =   375
   ClientTop       =   315
   ClientWidth     =   8190
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   ScaleHeight     =   6450
   ScaleWidth      =   8190
   Begin VB.TextBox Text13 
      ForeColor       =   &H00C000C0&
      Height          =   285
      Left            =   6840
      Locked          =   -1  'True
      TabIndex        =   32
      Top             =   4440
      Width           =   1155
   End
   Begin VB.PictureBox Picture3 
      Appearance      =   0  'Flat
      AutoRedraw      =   -1  'True
      BackColor       =   &H80000005&
      ForeColor       =   &H80000008&
      Height          =   1455
      Left            =   180
      ScaleHeight     =   1425
      ScaleWidth      =   7785
      TabIndex        =   31
      Top             =   2880
      Width           =   7815
   End
   Begin VB.Frame Frame2 
      Caption         =   "Iteraciones"
      Height          =   1635
      Left            =   1860
      TabIndex        =   20
      Top             =   4800
      Width           =   3135
      Begin VB.TextBox Text12 
         Height          =   285
         Left            =   2100
         TabIndex        =   29
         Top             =   1200
         Width           =   735
      End
      Begin VB.TextBox Text11 
         Height          =   285
         Left            =   900
         TabIndex        =   24
         Top             =   720
         Width           =   735
      End
      Begin VB.TextBox Text10 
         Height          =   285
         Left            =   2100
         TabIndex        =   23
         Top             =   300
         Width           =   735
      End
      Begin VB.TextBox Text9 
         Height          =   285
         Left            =   2100
         TabIndex        =   22
         Top             =   600
         Width           =   735
      End
      Begin VB.TextBox Text8 
         Height          =   285
         Left            =   2100
         TabIndex        =   21
         Top             =   900
         Width           =   735
      End
      Begin VB.Label Label12 
         AutoSize        =   -1  'True
         Caption         =   "P"
         Height          =   195
         Left            =   1920
         TabIndex        =   30
         Top             =   1260
         Width           =   105
      End
      Begin VB.Label Label11 
         AutoSize        =   -1  'True
         Caption         =   "Máxima"
         Height          =   195
         Left            =   180
         TabIndex        =   28
         Top             =   780
         Width           =   540
      End
      Begin VB.Label Label10 
         AutoSize        =   -1  'True
         Caption         =   "T"
         Height          =   195
         Left            =   1920
         TabIndex        =   27
         Top             =   300
         Width           =   105
      End
      Begin VB.Label Label9 
         AutoSize        =   -1  'True
         Caption         =   "u"
         Height          =   195
         Left            =   1920
         TabIndex        =   26
         Top             =   600
         Width           =   90
      End
      Begin VB.Label Label8 
         AutoSize        =   -1  'True
         Caption         =   "v"
         Height          =   195
         Left            =   1920
         TabIndex        =   25
         Top             =   900
         Width           =   90
      End
   End
   Begin VB.Frame Frame1 
      Caption         =   "Relajación"
      Height          =   1635
      Left            =   240
      TabIndex        =   11
      Top             =   4800
      Width           =   1455
      Begin VB.TextBox Text7 
         Height          =   285
         Left            =   420
         TabIndex        =   19
         Top             =   1200
         Width           =   735
      End
      Begin VB.TextBox Text6 
         Height          =   285
         Left            =   420
         TabIndex        =   17
         Top             =   900
         Width           =   735
      End
      Begin VB.TextBox Text5 
         Height          =   285
         Left            =   420
         TabIndex        =   15
         Top             =   600
         Width           =   735
      End
      Begin VB.TextBox Text4 
         Height          =   285
         Left            =   420
         TabIndex        =   13
         Top             =   300
         Width           =   735
      End
      Begin VB.Label Label7 
         AutoSize        =   -1  'True
         Caption         =   "v"
         Height          =   195
         Left            =   180
         TabIndex        =   18
         Top             =   1260
         Width           =   90
      End
      Begin VB.Label Label6 
         AutoSize        =   -1  'True
         Caption         =   "u"
         Height          =   195
         Left            =   180
         TabIndex        =   16
         Top             =   960
         Width           =   90
      End
      Begin VB.Label Label5 
         AutoSize        =   -1  'True
         Caption         =   "P"
         Height          =   195
         Left            =   180
         TabIndex        =   14
         Top             =   660
         Width           =   105
      End
      Begin VB.Label Label4 
         AutoSize        =   -1  'True
         Caption         =   "T"
         Height          =   195
         Left            =   180
         TabIndex        =   12
         Top             =   360
         Width           =   105
      End
   End
   Begin VB.CommandButton Command3 
      Caption         =   "Post"
      Height          =   375
      Left            =   6180
      TabIndex        =   6
      Top             =   5940
      Width           =   975
   End
   Begin VB.CommandButton Command2 
      Caption         =   "Pre"
      Height          =   375
      Left            =   6180
      TabIndex        =   5
      Top             =   5100
      Width           =   975
   End
   Begin VB.TextBox Text3 
      ForeColor       =   &H00C00000&
      Height          =   285
      Left            =   4620
      Locked          =   -1  'True
      TabIndex        =   4
      Top             =   4440
      Width           =   1155
   End
   Begin VB.TextBox Text2 
      ForeColor       =   &H000000C0&
      Height          =   285
      Left            =   2460
      Locked          =   -1  'True
      TabIndex        =   3
      Top             =   4440
      Width           =   1155
   End
   Begin VB.TextBox Text1 
      Height          =   285
      Left            =   840
      Locked          =   -1  'True
      TabIndex        =   2
      Top             =   4440
      Width           =   615
   End
   Begin VB.CommandButton Command1 
      Caption         =   "Calcular"
      Height          =   375
      Left            =   6180
      TabIndex        =   1
      Top             =   5520
      Width           =   975
   End
   Begin VB.PictureBox Picture1 
      Appearance      =   0  'Flat
      AutoRedraw      =   -1  'True
      BackColor       =   &H80000005&
      ForeColor       =   &H80000008&
      Height          =   1455
      Left            =   180
      ScaleHeight     =   1425
      ScaleWidth      =   7785
      TabIndex        =   0
      Top             =   0
      Width           =   7815
   End
   Begin VB.PictureBox Picture2 
      Appearance      =   0  'Flat
      AutoRedraw      =   -1  'True
      BackColor       =   &H80000005&
      ForeColor       =   &H80000008&
      Height          =   1455
      Left            =   180
      ScaleHeight     =   1425
      ScaleWidth      =   7785
      TabIndex        =   7
      Top             =   1440
      Width           =   7815
   End
   Begin VB.Timer Timer1 
      Left            =   3720
      Top             =   1920
   End
   Begin VB.Label Label13 
      Caption         =   "RTmax.(ulti.)"
      ForeColor       =   &H00C000C0&
      Height          =   195
      Left            =   5880
      TabIndex        =   33
      Top             =   4500
      Width           =   915
   End
   Begin VB.Label Label3 
      Caption         =   "SSum (ulti.)"
      ForeColor       =   &H00C00000&
      Height          =   195
      Left            =   3720
      TabIndex        =   10
      Top             =   4500
      Width           =   795
   End
   Begin VB.Label Label2 
      Caption         =   "SMax (ulti.)"
      ForeColor       =   &H000000C0&
      Height          =   195
      Left            =   1560
      TabIndex        =   9
      Top             =   4500
      Width           =   795
   End
   Begin VB.Label Label1 
      Caption         =   "Nº Iter."
      Height          =   195
      Left            =   240
      TabIndex        =   8
      Top             =   4500
      Width           =   555
   End
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False

Sub Graf()
Dim Maxv As Double, Minv As Double, Mx As Double, My As Double, _
Verdad As Boolean
Verdad = False
For i = 0 To 2
        eee = Mid(Solve, (i * 2 + 1), 1)
        If eee <> "F" Then
            Select Case i
                Case 0, 1
                    Verdad = True
            End Select
        End If
Next i
If Verdad Then
    Maxv = Maxx(SMax)
    Minv = Minn(SMax)
    My = (Maxv - Minv)
    With Picture1
        .ScaleMode = vbPixels
        Ex = (Iter - 1) / (.ScaleWidth - 10)
        Ey = IIf(My > 0, My / (.ScaleHeight - 10), 1)
        .Cls
    End With
    Picture1.Line (5, Picture1.ScaleHeight - 5)-(5, 5)
    Picture1.Line (5, Picture1.ScaleHeight - 5)-(Picture1.ScaleWidth - 5, Picture1.ScaleHeight - 5)
    Picture1.PSet (5, Picture1.ScaleHeight - (SMax(1) - Minv) / Ey - 5)
    For i = 3 To Iter
        Picture1.Line -(5 + (i - 1) / Ex, Picture1.ScaleHeight - (SMax(i) - Minv) / Ey - 5), vbRed
    Next i
    Maxv = Maxx(SSum)
    Minv = Minn(SSum)
    My = (Maxv - Minv)
    With Picture2
        .ScaleMode = vbPixels
        Ex = (Iter - 1) / (.ScaleWidth - 10)
        Ey = IIf(My > 0, My / (.ScaleHeight - 10), 1)
        .Cls
    End With
    Picture2.Line (5, Picture2.ScaleHeight - 5)-(5, 5)
'    Picture2.Line (5, Picture2.ScaleHeight + Minv / Ey - 5)-(Picture2.ScaleWidth - 5, Picture2.ScaleHeight + Minv / Ey - 5)
    Picture2.PSet (5, Picture2.ScaleHeight - (SSum(1) - Minv) / Ey - 5)
    For i = 3 To Iter
        Picture2.Line -(5 + (i - 1) / Ex, Picture2.ScaleHeight - (SSum(i) - Minv) / Ey - 5), vbBlue
    Next i
End If
    Maxv = Maxx(RT)
    Minv = Minn(RT)
    My = (Maxv - Minv)
    With Picture3
        .ScaleMode = vbPixels
        Ex = (Iter - 1) / (.ScaleWidth - 10)
        Ey = IIf(My > 0, My / (.ScaleHeight - 10), 1)
        .Cls
    End With
    Picture3.Line (5, Picture3.ScaleHeight - 5)-(5, 5)
'    Picture2.Line (5, Picture2.ScaleHeight + Minv / Ey - 5)-(Picture2.ScaleWidth - 5, Picture2.ScaleHeight + Minv / Ey - 5)
    Picture3.PSet (5, Picture3.ScaleHeight - (RT(1) - Minv) / Ey - 5)
    For i = 3 To Iter
        Picture3.Line -(5 + (i - 1) / Ex, Picture3.ScaleHeight - (RT(i) - Minv) / Ey - 5), vbMagenta
    Next i


End Sub


Static Function Maxx(h)
Dim F As Variant
F = (h(1))
For P = 2 To UBound(h)
    If (h(P)) > F Then F = (h(P))
Next P
Maxx = F
End Function
Static Function Minn(h)
Dim F As Variant
F = (h(1))
For P = 2 To UBound(h)
    If (h(P)) < F Then F = (h(P))
Next P
Minn = F

End Function

Private Sub Command1_Click()
Nc = Nc + 1
Erase SSum, SMax
Open "param.el" For Output As #1
    Print #1, "F"
    Print #1, Ntime(0)
    Print #1, Ntime(2), Ntime(3), Ntime(1), Ntime(4)
    Print #1, Relax(1), Relax(2), Relax(0), Relax(3)
    Print #1, 100
Close #1
Open "Pausa.el" For Output As #1
     Print #1, 1
Close #1
If Dir("error.el") <> "" Then Kill ("error.el")
    A = Shell("solver.exe", vbHide)
    Iter = 0
10    If Dir("error.el") <> "" Then
'        For yy = 0 To 10000: DoEvents: Next yy
        Timer1.Interval = 10
        Command1.Enabled = False
        Frame1.Enabled = False
        Frame2.Enabled = False
    Else
        GoTo 10
    End If
End Sub

Private Sub Command2_Click()
AppActivate "Pre-Procesador"
End
End Sub

Private Sub Command3_Click()
id = Shell("Post.exe", vbNormalFocus)
End
End Sub

Private Sub Form_Load()
    Open "geom.el" For Input As #1
        Input #1, Mode
        Input #1, Xl, Yl, R1
        Input #1, L1, M1
        Solve = ""
        Input #1, Solve
        Close #1
        For i = 0 To 2
            eee = Mid(Solve, (i * 2 + 1), 1)
            If eee = "F" Then
                Select Case i
                    Case 0
                         Text5.Visible = False
                         Text6.Visible = False
                         Text9.Visible = False
                         Text12.Visible = False
                         Label5.Visible = False
                         Label7.Visible = False
                         Label9.Visible = False
                         Label12.Visible = False
                    Case 1
                         Text5.Visible = False
                         Text7.Visible = False
                         Text8.Visible = False
                         Text12.Visible = False
                         Label6.Visible = False
                         Label5.Visible = False
                         Label8.Visible = False
                         Label12.Visible = False
                     Case 2
                        Text4.Visible = False
                        Label4.Visible = False
                        Text10.Visible = False
                        Label10.Visible = False
                        
                End Select
            Else
                Select Case i
                    Case 0
                         Text5.Visible = True
                         Text6.Visible = True
                         Text7.Visible = True
                         Text9.Visible = True
                         Text8.Visible = True
                         Text12.Visible = True
                         Label6.Visible = True
                         Label5.Visible = True
                         Label7.Visible = True
                         Label9.Visible = True
                         Label8.Visible = True
                         Label12.Visible = True
                    Case 1
                         Text5.Visible = True
                         Text6.Visible = True
                         Text7.Visible = True
                         Text9.Visible = True
                         Text8.Visible = True
                         Text12.Visible = True
                         Label6.Visible = True
                         Label5.Visible = True
                         Label7.Visible = True
                         Label9.Visible = True
                         Label8.Visible = True
                         Label12.Visible = True
                    Case 2
                         Text4.Visible = True
                         Label4.Visible = True
                         Text10.Visible = True
                        Label10.Visible = True
                End Select
            End If
    Next i
Nc = 0
   'Open "error.el" For Input As #1
   '     Do
   '         Input #1, iter
   '         ReDim Preserve ssum(iter), smax(iter)
   '         Input #1, ssum(iter), smax(iter)
   '     Loop Until EOF(1)
   ' Close #1
    Open "param.el" For Input As #1
        Input #1, Tt
        Input #1, Tt
    Close #1
    Open "param.el" For Input As #1
        Input #1, cx
        Input #1, Ntime(0)
        Input #1, Ntime(2), Ntime(3), Ntime(1), Ntime(4)
        Input #1, Relax(1), Relax(2), Relax(0), Relax(3)
        Input #1, cx
Close #1
Text4 = Relax(0)
Text5 = Relax(3)
Text6 = Relax(1)
Text7 = Relax(2)
Text11 = Ntime(0)
Text12 = Ntime(4)
Text10 = Ntime(1)
Text9 = Ntime(2)
Text8 = Ntime(3)
End Sub


Private Sub Form_Unload(Cancel As Integer)
End
End Sub


Private Sub Text10_Change()
'Text4 = Relax(0)
'Text5 = Relax(3)
'Text6 = Relax(1)
'Text7 = Relax(2)
'Text11 = Ntime(0)
'Text12 = Ntime(4)
  Ntime(1) = Val(Text10)
'Text9 = Ntime(2)
'Text8 = Ntime(3)

End Sub

Private Sub Text11_Change()
'Text4 = Relax(0)
'Text5 = Relax(3)
'Text6 = Relax(1)
'Text7 = Relax(2)
 Ntime(0) = Val(Text11)
 Tt = Ntime(0)
'Text12 = Ntime(4)
'Text10 = Ntime(1)
'Text9 = Ntime(2)
'Text8 = Ntime(3)

End Sub


Private Sub Text12_Change()
'Text4 = Relax(0)
'Text5 = Relax(3)
'Text6 = Relax(1)
'Text7 = Relax(2)
'Text11 = Ntime(0)
  Ntime(4) = Val(Text12)
'Text10 = Ntime(1)
'Text9 = Ntime(2)
'Text8 = Ntime(3)

End Sub


Private Sub Text4_Change()
Relax(0) = Val(Text4)
'Text5 = Relax(3)
'Text6 = Relax(1)
'Text7 = Relax(2)
'Text11 = Ntime(0)
'Text12 = Ntime(4)
'Text10 = Ntime(1)
'Text9 = Ntime(2)
'Text8 = Ntime(3)

End Sub


Private Sub Text5_Change()
'Text4 = Relax(0)
Relax(3) = Val(Text5)
'Text6 = Relax(1)
'Text7 = Relax(2)
'Text11 = Ntime(0)
'Text12 = Ntime(4)
'Text10 = Ntime(1)
'Text9 = Ntime(2)
'Text8 = Ntime(3)
'
End Sub


Private Sub Text6_Change()
'Text4 = Relax(0)
'Text5 = Relax(3)
Relax(1) = Val(Text6)
'Text7 = Relax(2)
'Text11 = Ntime(0)
'Text12 = Ntime(4)
'Text10 = Ntime(1)
'Text9 = Ntime(2)
'Text8 = Ntime(3)

End Sub


Private Sub Text7_Change()
'Text4 = Relax(0)
'Text5 = Relax(3)
'Text6 = Relax(1)
 Relax(2) = Val(Text7)
'Text11 = Ntime(0)
'Text12 = Ntime(4)
'Text10 = Ntime(1)
'Text9 = Ntime(2)
'Text8 = Ntime(3)

End Sub


Private Sub Text8_Change()
'Text4 = Relax(0)
'Text5 = Relax(3)
'Text6 = Relax(1)
'Text7 = Relax(2)
'Text11 = Ntime(0)
'Text12 = Ntime(4)
'Text10 = Ntime(1)
'Text9 = Ntime(2)
  Ntime(3) = Val(Text8)

End Sub


Private Sub Text9_Change()
'Text4 = Relax(0)
'Text5 = Relax(3)
'Text6 = Relax(1)
'Text7 = Relax(2)
'Text11 = Ntime(0)
'Text12 = Ntime(4)
'Text10 = Ntime(1)
  Ntime(2) = Val(Text9)
'Text8 = Ntime(3)

End Sub


Private Sub Timer1_Timer()
    On Error GoTo 10

        If Iter <> Tt Then
            Open "error.el" For Input As #1
                Do Until EOF(1)
                     Input #1, Iter
                     ReDim Preserve SSum(Iter), SMax(Iter), RT(Iter)
                     Input #1, SSum(Iter), SMax(Iter), RT(Iter)
                 Loop
             Close #1
             If Iter <> 0 Then
                Text1 = Iter
                Text3 = Format(SSum(Iter), "0.000e+##")
                Text2 = Format(SMax(Iter), "0.000e+##")
                Text13 = Format(RT(Iter), "0.000e+##")
                Debug.Print SMax(Iter)
                Debug.Print SSum(Iter)
                Debug.Print
                
                Call Graf
             End If
        Else
            Command1.Enabled = True
            Frame1.Enabled = True
            Frame2.Enabled = True
            Timer1.Interval = 0
        End If
    Exit Sub
10     Close #1
End Sub


