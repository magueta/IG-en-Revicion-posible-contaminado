VERSION 5.00
Begin VB.Form Fver 
   AutoRedraw      =   -1  'True
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Pre-Procesador"
   ClientHeight    =   6180
   ClientLeft      =   1035
   ClientTop       =   435
   ClientWidth     =   7860
   LinkTopic       =   "Form2"
   LockControls    =   -1  'True
   MaxButton       =   0   'False
   MinButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   6180
   ScaleWidth      =   7860
   Begin VB.CommandButton Command9 
      Caption         =   "Nuevo"
      Height          =   495
      Left            =   6360
      TabIndex        =   10
      Top             =   4980
      Width           =   1215
   End
   Begin VB.CommandButton Command8 
      Caption         =   "Resolver"
      Height          =   495
      Left            =   6360
      TabIndex        =   9
      Top             =   4440
      Width           =   1215
   End
   Begin VB.CommandButton Command6 
      Caption         =   "Parametros relacionados"
      Height          =   495
      Left            =   6360
      TabIndex        =   7
      Top             =   2760
      Width           =   1215
   End
   Begin VB.CommandButton Command7 
      Caption         =   " Propiedades        del Medio"
      Height          =   495
      Left            =   6360
      TabIndex        =   6
      Top             =   1680
      Width           =   1215
   End
   Begin VB.CommandButton Command5 
      Caption         =   "  Condiciones       de fronteras"
      Height          =   495
      Left            =   6360
      TabIndex        =   5
      Top             =   2220
      Width           =   1215
   End
   Begin VB.CommandButton Command4 
      Caption         =   "Mallado"
      Height          =   495
      Left            =   6360
      TabIndex        =   4
      Top             =   1140
      Width           =   1215
   End
   Begin VB.CommandButton Command3 
      Caption         =   "Salir"
      Height          =   495
      Left            =   6360
      TabIndex        =   3
      Top             =   5520
      Width           =   1215
   End
   Begin VB.CommandButton Command2 
      Caption         =   "  Editar Sólidos"
      Height          =   495
      Left            =   6360
      TabIndex        =   2
      Top             =   600
      Width           =   1215
   End
   Begin VB.CommandButton Command1 
      Caption         =   "Agregar Sólidos"
      Height          =   495
      Left            =   6360
      TabIndex        =   1
      Top             =   60
      Width           =   1215
   End
   Begin VB.PictureBox Picture1 
      Appearance      =   0  'Flat
      AutoRedraw      =   -1  'True
      BackColor       =   &H00000000&
      FillColor       =   &H00800000&
      ForeColor       =   &H00FFFFFF&
      Height          =   6015
      Left            =   60
      ScaleHeight     =   5985
      ScaleWidth      =   5985
      TabIndex        =   0
      Top             =   60
      Width           =   6015
   End
   Begin VB.Label Label1 
      BorderStyle     =   1  'Fixed Single
      Caption         =   "Label1"
      Height          =   1095
      Left            =   6360
      TabIndex        =   8
      Top             =   3300
      Width           =   1215
   End
End
Attribute VB_Name = "Fver"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False



Sub CEscala()
  VBCP_Update 15, "CEscala", 1
    EscalaXY = Xl
  VBCP_Update 15, "CEscala", 2
    If Yl > Xl And VBCP_UpdateIf(15, "CEscala", 3) Then
        EscalaXY = Yl
  VBCP_Update 15, "CEscala", 4
    End If
    Mediaescala = (EscalaXY * 1.1) / 2
  VBCP_Update 15, "CEscala", 5
    With Picture1
        .ScaleHeight = -2 * Mediaescala
  VBCP_Update 15, "CEscala", 6
        .ScaleWidth = 2 * Mediaescala
  VBCP_Update 15, "CEscala", 7
        .ScaleLeft = -Mediaescala + Xl / 2
  VBCP_Update 15, "CEscala", 8
        .ScaleTop = Yl / 2 + Mediaescala
  VBCP_Update 15, "CEscala", 9
    End With
End Sub

Sub DiDominio()
  VBCP_Update 15, "DiDominio", 1
    Picture1.Cls
  VBCP_Update 15, "DiDominio", 2
    Picture1.Line (0, 0)-(Xl, Yl), , B
  VBCP_Update 15, "DiDominio", 3
End Sub
Sub DiMalla()
  VBCP_Update 15, "DiMalla", 1
    For I = 1 To L1
  VBCP_Update 15, "DiMalla", 2
         For J = 1 To M1
  VBCP_Update 15, "DiMalla", 3
             Picture1.PSet (X(I), Y(J)), QBColor(12)
  VBCP_Update 15, "DiMalla", 4
         Next J
     Next I
     For I = 2 To L2
  VBCP_Update 15, "DiMalla", 5
         For J = 2 To M2
  VBCP_Update 15, "DiMalla", 6
             Picture1.Line (XU(I), YV(J))-(XU(I), YV(J + 1)), QBColor(8)
  VBCP_Update 15, "DiMalla", 7
             Picture1.Line (XU(I), YV(J))-(XU(I + 1), YV(J)), QBColor(8)
  VBCP_Update 15, "DiMalla", 8
         Next J
     Next I
    Picture1.Line (XU(L1), YV(M1))-(XU(L1), YV(2)), QBColor(8)
  VBCP_Update 15, "DiMalla", 9
    Picture1.Line (XU(2), YV(M1))-(XU(L1), YV(M1)), QBColor(8)
  VBCP_Update 15, "DiMalla", 10
End Sub

Sub DiSolido()
  VBCP_Update 15, "DiSolido", 1
    If Ns > 0 And VBCP_UpdateIf(15, "DiSolido", 2) Then
        For K = 1 To Ns
  VBCP_Update 15, "DiSolido", 3
            Picture1.Line (S(0, K), S(1, K))-(S(2, K), S(3, K)), (vbMagenta), B
  VBCP_Update 15, "DiSolido", 4
            Picture1.CurrentX = S(0, K) / 2 + S(2, K) / 2
  VBCP_Update 15, "DiSolido", 5
            Picture1.CurrentY = S(1, K) / 2 + S(3, K) / 2
  VBCP_Update 15, "DiSolido", 6
            Picture1.Print K
  VBCP_Update 15, "DiSolido", 7
        Next K
    End If
End Sub

Sub Ocgxykr()
  VBCP_Update 15, "Ocgxykr", 1
'Optener k
For J = 1 To M1
  VBCP_Update 15, "Ocgxykr", 2
    For I = 1 To L1
  VBCP_Update 15, "Ocgxykr", 3
        RHO(I, J) = Pro(0)
  VBCP_Update 15, "Ocgxykr", 4
        rK(I, J) = Pro(1)
  VBCP_Update 15, "Ocgxykr", 5
        Be(I, J) = Pro(2)
  VBCP_Update 15, "Ocgxykr", 6
        CP(I, J) = Pro(3)
  VBCP_Update 15, "Ocgxykr", 7
        GBX(I, J) = Gx * Pro(4)
  VBCP_Update 15, "Ocgxykr", 8
        GBY(I, J) = Gy * Pro(4)
  VBCP_Update 15, "Ocgxykr", 9
        For K = 1 To Ns
  VBCP_Update 15, "Ocgxykr", 10
            If (S(0, K) <= X(I) And S(2, K) >= X(I)) And (S(1, K) <= Y(J) And S(3, K) >= Y(J)) _
            Then
  VBCP_Update 15, "Ocgxykr", 11
                RHO(I, J) = S(5, K)
                rK(I, J) = S(4, K)
  VBCP_Update 15, "Ocgxykr", 12
                Be(I, J) = 1E+38
  VBCP_Update 15, "Ocgxykr", 13
                CP(I, J) = S(6, K)
  VBCP_Update 15, "Ocgxykr", 14
            End If
        Next K
    Next I
Next J




End Sub

Sub OTFE()
  VBCP_Update 15, "OTFE", 1
ReDim TSp(L1, M1), TSc(L1, M1)
  VBCP_Update 15, "OTFE", 2
    For K = 1 To Ndx * Ndy
  VBCP_Update 15, "OTFE", 3
        Ji = (K - 1) \ Ndx + 1
  VBCP_Update 15, "OTFE", 4
        II = K - (Ji - 1) * Ndx
  VBCP_Update 15, "OTFE", 5
    For J = MM(Ji - 1) To MM(Ji) - 1
  VBCP_Update 15, "OTFE", 6
    For I = LL(II - 1) To LL(II) - 1
  VBCP_Update 15, "OTFE", 7
        TSp(I, J) = TFE(2, K)
  VBCP_Update 15, "OTFE", 8
        TSc(I, J) = TFE(1, K)
  VBCP_Update 15, "OTFE", 9
    Next I
    Next J
    Next K
End Sub

Sub OTUV()
  VBCP_Update 15, "OTUV", 1
For J = 2 To M2
  VBCP_Update 15, "OTUV", 2
    For I = 2 To L2
  VBCP_Update 15, "OTUV", 3
        T(I, J) = VI(0)
  VBCP_Update 15, "OTUV", 4
        If FU.Check1.Value = vbChecked And VBCP_UpdateIf(15, "OTUV", 5) Then
            U(I, J) = VI(1)
  VBCP_Update 15, "OTUV", 6
        Else
            U(I, J) = 0
  VBCP_Update 15, "OTUV", 7
        End If
        
        If FU.Check1.Value = vbChecked And VBCP_UpdateIf(15, "OTUV", 8) Then
            V(I, J) = VI(2)
  VBCP_Update 15, "OTUV", 9
        Else
            V(I, J) = 0
  VBCP_Update 15, "OTUV", 10
        End If
        
        For K = 1 To Ns
  VBCP_Update 15, "OTUV", 11
            If (S(0, K) <= X(I) And S(2, K) >= X(I)) And (S(1, K) <= Y(J) And S(3, K) >= Y(J)) _
                Then
  VBCP_Update 15, "OTUV", 12
                V(I + 1, J) = 0
                U(I, J + 1) = 0
  VBCP_Update 15, "OTUV", 13
                U(I, J) = 0
  VBCP_Update 15, "OTUV", 14
                V(I, J) = 0
  VBCP_Update 15, "OTUV", 15
            End If
        Next K
    Next I
Next J
For K = 1 To Ndxi
  VBCP_Update 15, "OTUV", 16
    
    If Txi(1, K) = 1 And VBCP_UpdateIf(15, "OTUV", 17) Then
        For I = Xi(1, K) + 1 To Xi(2, K) + 1
  VBCP_Update 15, "OTUV", 18
            T(I, 1) = Txi(2, K)
  VBCP_Update 15, "OTUV", 19
        Next I
    End If
    If Txi(1, K) = 4 And VBCP_UpdateIf(15, "OTUV", 20) Then
        For I = Xi(1, K) + 1 To Xi(2, K) + 1
  VBCP_Update 15, "OTUV", 21
            T(I, 1) = DL(Txi(3, K), Txi(2, K), X(Xi(1, K) + 1), X(Xi(2, K) + 1), X(I))
  VBCP_Update 15, "OTUV", 22
        Next I
    End If

    If Vxi(1, K) = 1 And VBCP_UpdateIf(15, "OTUV", 23) Then
        For I = Xi(1, K) + 1 To Xi(2, K) + 1
  VBCP_Update 15, "OTUV", 24
            V(I, 2) = DP(Vxi(2, K), Vxi(3, K), X(Xi(1, K) + 1), X(Xi(2, K) + 1), X(I))
  VBCP_Update 15, "OTUV", 25
        Next I
    End If
    If Uxi(1, K) = 2 And VBCP_UpdateIf(15, "OTUV", 26) Then
        For I = Xi(1, K) + 1 To Xi(2, K) + 1
  VBCP_Update 15, "OTUV", 27
            U(I, 1) = Uxi(2, K)
  VBCP_Update 15, "OTUV", 28
        Next I
    End If
    If Vxi(1, K) = 2 And VBCP_UpdateIf(15, "OTUV", 29) Then
        For I = Xi(1, K) + 1 To Xi(2, K) + 1
  VBCP_Update 15, "OTUV", 30
            V(I, 2) = Vxi(2, K)
  VBCP_Update 15, "OTUV", 31
        Next I
    End If
Next K


For K = 1 To Ndxs
  VBCP_Update 15, "OTUV", 32
    If Txs(1, K) = 1 And VBCP_UpdateIf(15, "OTUV", 33) Then
        For I = Xs(1, K) + 1 To Xs(2, K) + 1
  VBCP_Update 15, "OTUV", 34
            T(I, M1) = Txs(2, K)
  VBCP_Update 15, "OTUV", 35
        Next I
    End If
    If Txs(1, K) = 4 And VBCP_UpdateIf(15, "OTUV", 36) Then
        For I = Xs(1, K) + 1 To Xs(2, K) + 1
  VBCP_Update 15, "OTUV", 37
            T(I, M1) = DL(Txs(3, K), Txs(2, K), X(Xs(1, K) + 1), X(Xs(2, K) + 1), X(I))
  VBCP_Update 15, "OTUV", 38
        Next I
    End If
    
    If Uxs(1, K) = 2 And VBCP_UpdateIf(15, "OTUV", 39) Then
        For I = Xs(1, K) + 1 To Xs(2, K) + 1
  VBCP_Update 15, "OTUV", 40
            U(I, M1) = Uxs(2, K)
  VBCP_Update 15, "OTUV", 41
        Next I
    End If
        If Vxs(1, K) = 1 And VBCP_UpdateIf(15, "OTUV", 42) Then
        For I = Xs(1, K) + 1 To Xs(2, K) + 1
  VBCP_Update 15, "OTUV", 43
            V(I, M1) = DP(Vxs(2, K), Vxs(3, K), X(Xs(1, K) + 1), X(Xs(2, K) + 1), X(I))
  VBCP_Update 15, "OTUV", 44
        Next I
    End If
    
    If Vxs(1, K) = 2 And VBCP_UpdateIf(15, "OTUV", 45) Then
        For I = Xs(1, K) + 1 To Xs(2, K) + 1
  VBCP_Update 15, "OTUV", 46
            V(I, M1) = Vxs(2, K)
  VBCP_Update 15, "OTUV", 47
        Next I
    End If
Next K

For K = 1 To Ndyi
  VBCP_Update 15, "OTUV", 48
    If Tyi(1, K) = 1 And VBCP_UpdateIf(15, "OTUV", 49) Then
        For I = Yi(1, K) + 1 To Yi(2, K) + 1
  VBCP_Update 15, "OTUV", 50
            T(1, I) = Tyi(2, K)
  VBCP_Update 15, "OTUV", 51
        Next I
    End If
    If Tyi(1, K) = 4 And VBCP_UpdateIf(15, "OTUV", 52) Then
        For I = Yi(1, K) + 1 To Yi(2, K) + 1
  VBCP_Update 15, "OTUV", 53
            T(1, I) = DL(Tyi(3, K), Tyi(2, K), Y(Yi(1, K) + 1), Y(Yi(2, K) + 1), Y(I))
  VBCP_Update 15, "OTUV", 54
        Next I
    End If
    If Uyi(1, K) = 1 And VBCP_UpdateIf(15, "OTUV", 55) Then
        For I = Yi(1, K) + 1 To Yi(2, K) + 1
  VBCP_Update 15, "OTUV", 56
            U(2, I) = DP(Uyi(2, K), Uyi(3, K), Y(Yi(1, K) + 1), Y(Yi(2, K) + 1), Y(I))
  VBCP_Update 15, "OTUV", 57
        Next I
    End If

    If Uyi(1, K) = 2 And VBCP_UpdateIf(15, "OTUV", 58) Then
        For I = Yi(1, K) + 1 To Yi(2, K) + 1
  VBCP_Update 15, "OTUV", 59
            U(2, I) = Uyi(2, K)
  VBCP_Update 15, "OTUV", 60
        Next I
    End If
    If Vyi(1, K) = 2 And VBCP_UpdateIf(15, "OTUV", 61) Then
        For I = Yi(1, K) + 1 To Yi(2, K) + 1
  VBCP_Update 15, "OTUV", 62
            V(1, I) = Vyi(2, K)
  VBCP_Update 15, "OTUV", 63
        Next I
    End If
Next K
For K = 1 To Ndyd
  VBCP_Update 15, "OTUV", 64
    If Tyd(1, K) = 1 And VBCP_UpdateIf(15, "OTUV", 65) Then
        For I = Yd(1, K) + 1 To Yd(2, K) + 1
  VBCP_Update 15, "OTUV", 66
            T(L1, I) = Tyd(2, K)
  VBCP_Update 15, "OTUV", 67
        Next I
    End If
    If Tyd(1, K) = 4 And VBCP_UpdateIf(15, "OTUV", 68) Then
        For I = Yd(1, K) + 1 To Yd(2, K) + 1
  VBCP_Update 15, "OTUV", 69
            T(L1, I) = DL(Tyd(3, K), Tyd(2, K), Y(Yd(1, K) + 1), Y(Yd(2, K) + 1), Y(I))
  VBCP_Update 15, "OTUV", 70
        Next I
    End If
    
    If Uyd(1, K) = 1 And VBCP_UpdateIf(15, "OTUV", 71) Then
        For I = Yd(1, K) + 1 To Yd(2, K) + 1
  VBCP_Update 15, "OTUV", 72
            U(L1, I) = DP(Uyd(2, K), Uyd(3, K), Y(Yd(1, K) + 1), Y(Yd(2, K) + 1), Y(I))
  VBCP_Update 15, "OTUV", 73
        Next I
    End If
    If Uyd(1, K) = 2 And VBCP_UpdateIf(15, "OTUV", 74) Then
        For I = Yd(1, K) + 1 To Yd(2, K) + 1
  VBCP_Update 15, "OTUV", 75
            U(L1, I) = Uyd(2, K)
  VBCP_Update 15, "OTUV", 76
        Next I
    End If
    If Vyd(1, K) = 2 And VBCP_UpdateIf(15, "OTUV", 77) Then
        For I = Yd(1, K) + 1 To Yd(2, K) + 1
  VBCP_Update 15, "OTUV", 78
            V(L1, I) = Vyd(2, K)
  VBCP_Update 15, "OTUV", 79
        Next I
    End If
Next K
End Sub

Private Sub Command1_Click()
  VBCP_Update 15, "Command1_Click", 1
    Fsolido.Show (1)
  VBCP_Update 15, "Command1_Click", 2
    DiDominio
  VBCP_Update 15, "Command1_Click", 3
    DiSolido
  VBCP_Update 15, "Command1_Click", 4
End Sub

Private Sub Command2_Click()
  VBCP_Update 15, "Command2_Click", 1

FBorrar.Show (1)
  VBCP_Update 15, "Command2_Click", 2
DiDominio
  VBCP_Update 15, "Command2_Click", 3
If M1 > 2 And L1 > 2 And VBCP_UpdateIf(15, "Command2_Click", 4) Then
    DiMalla
  VBCP_Update 15, "Command2_Click", 5
End If
DiSolido
  VBCP_Update 15, "Command2_Click", 6


End Sub

Private Sub Command3_Click()
  VBCP_Update 15, "Command3_Click", 1
End
End Sub


Private Sub Command4_Click()
  VBCP_Update 15, "Command4_Click", 1
    ObtenerS
  VBCP_Update 15, "Command4_Click", 2
    Dim K As Integer
    FMalla.Show (1)
  VBCP_Update 15, "Command4_Click", 3
    DiDominio
  VBCP_Update 15, "Command4_Click", 4
    DiMalla
  VBCP_Update 15, "Command4_Click", 5
    DiSolido
  VBCP_Update 15, "Command4_Click", 6
End Sub

Private Sub Command5_Click()
  VBCP_Update 15, "Command5_Click", 1
FCF.Show (1)
  VBCP_Update 15, "Command5_Click", 2

End Sub

Private Sub Command6_Click()
  VBCP_Update 15, "Command6_Click", 1
FPR.Show (1)
  VBCP_Update 15, "Command6_Click", 2

End Sub

Private Sub Command7_Click()
  VBCP_Update 15, "Command7_Click", 1
Fpro.Show (1)
  VBCP_Update 15, "Command7_Click", 2
End Sub

Private Sub Command8_Click()
  VBCP_Update 15, "Command8_Click", 1
ReDim T(L1, M1), U(L1, M1), V(L1, M1), RHO(L1, M1)
  VBCP_Update 15, "Command8_Click", 2
ReDim rK(L1, M1), Be(L1, M1), CP(L1, M1), GBX(L1, M1), GBY(L1, M1)
  VBCP_Update 15, "Command8_Click", 3
Static Solve As String
Call OTUV
  VBCP_Update 15, "Command8_Click", 4
Call Ocgxykr
  VBCP_Update 15, "Command8_Click", 5
Call OTFE
  VBCP_Update 15, "Command8_Click", 6
Open "solido.el" For Output As #1
  VBCP_Update 15, "Command8_Click", 7
Print #1, Ns
  VBCP_Update 15, "Command8_Click", 8
For I = 1 To Ns
  VBCP_Update 15, "Command8_Click", 9
    Print #1, S(o, I), S(1, I), S(2, I), S(3, I), S(4, I), S(5, I), S(6, I)
  VBCP_Update 15, "Command8_Click", 10
Next I
Print #1, Pro(1)
  VBCP_Update 15, "Command8_Click", 11

Close #1
  VBCP_Update 15, "Command8_Click", 12
Open "geom.el" For Output As #1
  VBCP_Update 15, "Command8_Click", 13
Print #1, Mode
  VBCP_Update 15, "Command8_Click", 14
Print #1, Xl, Yl, R1
  VBCP_Update 15, "Command8_Click", 15
Print #1, L1, M1
  VBCP_Update 15, "Command8_Click", 16
Solve = ""
  VBCP_Update 15, "Command8_Click", 17
If FU.Check1 = vbChecked And VBCP_UpdateIf(15, "Command8_Click", 18) Then Solve = "T " Else Solve = "F "
If FV.Check1 = vbChecked Then Solve = Solve & "T " _
                                        Else Solve = Solve & "F "
If FE.Check1 = vbChecked Then Solve = Solve & "T" _
                                        Else Solve = Solve & "F"
Print #1, Solve

For I = 2 To L1
  VBCP_Update 15, "Command8_Click", 19
    Print #1, XU(I)
  VBCP_Update 15, "Command8_Click", 20
Next I
For J = 2 To M1
  VBCP_Update 15, "Command8_Click", 21
    Print #1, YV(J)
  VBCP_Update 15, "Command8_Click", 22
Next J
Print #1, Simetria
  VBCP_Update 15, "Command8_Click", 23
Close #1
  VBCP_Update 15, "Command8_Click", 24

Open "param.el" For Output As #1
  VBCP_Update 15, "Command8_Click", 25
Print #1, "F"
  VBCP_Update 15, "Command8_Click", 26
Print #1, NTime(0)
  VBCP_Update 15, "Command8_Click", 27
Print #1, NTime(2), NTime(3), NTime(1), NTime(4)
  VBCP_Update 15, "Command8_Click", 28
Print #1, Relax(1), Relax(2), Relax(0), Relax(3)
  VBCP_Update 15, "Command8_Click", 29
Print #1, 100
  VBCP_Update 15, "Command8_Click", 30
Close #1
  VBCP_Update 15, "Command8_Click", 31


Open "t.el" For Output As #1
  VBCP_Update 15, "Command8_Click", 32
For J = 1 To M1
  VBCP_Update 15, "Command8_Click", 33
    For I = 1 To L1
  VBCP_Update 15, "Command8_Click", 34
        Print #1, T(I, J)
  VBCP_Update 15, "Command8_Click", 35
    Next I
Next J

Close #1
  VBCP_Update 15, "Command8_Click", 36
Open "v.el" For Output As #1
  VBCP_Update 15, "Command8_Click", 37
For J = 2 To M1
  VBCP_Update 15, "Command8_Click", 38
    For I = 1 To L1
  VBCP_Update 15, "Command8_Click", 39
        Print #1, V(I, J)
  VBCP_Update 15, "Command8_Click", 40
    Next I
Next J

Close #1
  VBCP_Update 15, "Command8_Click", 41
Open "u.el" For Output As #1
  VBCP_Update 15, "Command8_Click", 42
For J = 1 To M1
  VBCP_Update 15, "Command8_Click", 43
    For I = 2 To L1
  VBCP_Update 15, "Command8_Click", 44
        Print #1, U(I, J)
  VBCP_Update 15, "Command8_Click", 45
    Next I
Next J
Close #1
  VBCP_Update 15, "Command8_Click", 46
Open "p.el" For Output As #1
  VBCP_Update 15, "Command8_Click", 47
For J = 1 To M1
  VBCP_Update 15, "Command8_Click", 48
    For I = 1 To L1
  VBCP_Update 15, "Command8_Click", 49
        Print #1, 0
  VBCP_Update 15, "Command8_Click", 50
    Next I
Next J
Close #1
  VBCP_Update 15, "Command8_Click", 51
Open "gama.el" For Output As #1
  VBCP_Update 15, "Command8_Click", 52
For J = 1 To M1
  VBCP_Update 15, "Command8_Click", 53
    For I = 1 To L1
  VBCP_Update 15, "Command8_Click", 54
        Print #1, rK(I, J), CP(I, J)
  VBCP_Update 15, "Command8_Click", 55
    Next I
Next J
For J = 1 To M1
  VBCP_Update 15, "Command8_Click", 56
    For I = 1 To L1
  VBCP_Update 15, "Command8_Click", 57
        Print #1, Be(I, J)
  VBCP_Update 15, "Command8_Click", 58
    Next I
Next J
For J = 1 To M1
  VBCP_Update 15, "Command8_Click", 59
    For I = 1 To L1
  VBCP_Update 15, "Command8_Click", 60
        Print #1, GBX(I, J)
  VBCP_Update 15, "Command8_Click", 61
    Next I
Next J
For J = 1 To M1
  VBCP_Update 15, "Command8_Click", 62
    For I = 1 To L1
  VBCP_Update 15, "Command8_Click", 63
        Print #1, GBY(I, J)
  VBCP_Update 15, "Command8_Click", 64
    Next I
Next J

Close #1
  VBCP_Update 15, "Command8_Click", 65
Open "rho.el" For Output As #1
  VBCP_Update 15, "Command8_Click", 66
For J = 1 To M1
  VBCP_Update 15, "Command8_Click", 67
    For I = 1 To L1
  VBCP_Update 15, "Command8_Click", 68
        Print #1, RHO(I, J)
  VBCP_Update 15, "Command8_Click", 69
    Next I
Next J
Close #1
  VBCP_Update 15, "Command8_Click", 70
Open "termi.el" For Output As #1
  VBCP_Update 15, "Command8_Click", 71
For J = 1 To M1
  VBCP_Update 15, "Command8_Click", 72
    For I = 1 To L1
  VBCP_Update 15, "Command8_Click", 73
        Print #1, TSc(I, J), TSp(I, J)
  VBCP_Update 15, "Command8_Click", 74
    Next I
Next J

Close #1
  VBCP_Update 15, "Command8_Click", 75
Open "contor.el" For Output As #1
  VBCP_Update 15, "Command8_Click", 76
    Print #1, Ndxi, Ndyi, Ndxs, Ndyd
  VBCP_Update 15, "Command8_Click", 77
    For J = 1 To Ndxi
  VBCP_Update 15, "Command8_Click", 78
        For I = 1 To 2
  VBCP_Update 15, "Command8_Click", 79
            Print #1, Xi(I, J)
  VBCP_Update 15, "Command8_Click", 80
        Next I
        Select Case Txi(1, J)
            Case 0
  VBCP_Update 15, "Command8_Click", 81
                Print #1, 1
  VBCP_Update 15, "Command8_Click", 82
                Print #1, 0
  VBCP_Update 15, "Command8_Click", 83
                Print #1, 0
  VBCP_Update 15, "Command8_Click", 84
            Case 1
  VBCP_Update 15, "Command8_Click", 85
                Print #1, 0
  VBCP_Update 15, "Command8_Click", 86
                Print #1, 0
  VBCP_Update 15, "Command8_Click", 87
                Print #1, 0
  VBCP_Update 15, "Command8_Click", 88
            Case 2
  VBCP_Update 15, "Command8_Click", 89
                Print #1, 1
  VBCP_Update 15, "Command8_Click", 90
                Print #1, Txi(2, J)
  VBCP_Update 15, "Command8_Click", 91
                Print #1, 0
  VBCP_Update 15, "Command8_Click", 92
            Case 4
  VBCP_Update 15, "Command8_Click", 93
                Print #1, 0
  VBCP_Update 15, "Command8_Click", 94
                Print #1, 0
  VBCP_Update 15, "Command8_Click", 95
                Print #1, 0
  VBCP_Update 15, "Command8_Click", 96
            Case 3
  VBCP_Update 15, "Command8_Click", 97
                Print #1, 4
  VBCP_Update 15, "Command8_Click", 98
                Print #1, Txi(2, J)
  VBCP_Update 15, "Command8_Click", 99
                Print #1, Txi(3, J)
  VBCP_Update 15, "Command8_Click", 100
        End Select
        If Vxi(1, J) = 0 And VBCP_UpdateIf(15, "Command8_Click", 101) Then Print #1, 1 Else Print #1, 0
    Next J
    For J = 1 To Ndyi
  VBCP_Update 15, "Command8_Click", 102
        For I = 1 To 2
  VBCP_Update 15, "Command8_Click", 103
            Print #1, Yi(I, J)
  VBCP_Update 15, "Command8_Click", 104
        Next I
        Select Case Tyi(1, J)
            Case 0
  VBCP_Update 15, "Command8_Click", 105
                Print #1, 1
  VBCP_Update 15, "Command8_Click", 106
                Print #1, 0
  VBCP_Update 15, "Command8_Click", 107
                Print #1, 0
  VBCP_Update 15, "Command8_Click", 108
            Case 1
  VBCP_Update 15, "Command8_Click", 109
                Print #1, 0
  VBCP_Update 15, "Command8_Click", 110
                Print #1, 0
  VBCP_Update 15, "Command8_Click", 111
                Print #1, 0
  VBCP_Update 15, "Command8_Click", 112
            Case 2
  VBCP_Update 15, "Command8_Click", 113
                Print #1, 1
  VBCP_Update 15, "Command8_Click", 114
                Print #1, Tyi(2, J)
  VBCP_Update 15, "Command8_Click", 115
                Print #1, 0
  VBCP_Update 15, "Command8_Click", 116
            Case 4
  VBCP_Update 15, "Command8_Click", 117
                Print #1, 0
  VBCP_Update 15, "Command8_Click", 118
                Print #1, 0
  VBCP_Update 15, "Command8_Click", 119
                Print #1, 0
  VBCP_Update 15, "Command8_Click", 120
            Case 3
  VBCP_Update 15, "Command8_Click", 121
                Print #1, 4
  VBCP_Update 15, "Command8_Click", 122
                Print #1, Tyi(2, J)
  VBCP_Update 15, "Command8_Click", 123
                Print #1, Tyi(3, J)
  VBCP_Update 15, "Command8_Click", 124
        End Select
        If Uyi(1, J) = 0 And VBCP_UpdateIf(15, "Command8_Click", 125) Then Print #1, 1 Else Print #1, 0
    Next J
    For J = 1 To Ndxs
  VBCP_Update 15, "Command8_Click", 126
        For I = 1 To 2
  VBCP_Update 15, "Command8_Click", 127
            Print #1, Xs(I, J)
  VBCP_Update 15, "Command8_Click", 128
        Next I
        Select Case Txs(1, J)
            Case 0
  VBCP_Update 15, "Command8_Click", 129
                Print #1, 1
  VBCP_Update 15, "Command8_Click", 130
                Print #1, 0
  VBCP_Update 15, "Command8_Click", 131
                Print #1, 0
  VBCP_Update 15, "Command8_Click", 132
            Case 1
  VBCP_Update 15, "Command8_Click", 133
                Print #1, 0
  VBCP_Update 15, "Command8_Click", 134
                Print #1, 0
  VBCP_Update 15, "Command8_Click", 135
                Print #1, 0
  VBCP_Update 15, "Command8_Click", 136
            Case 2
  VBCP_Update 15, "Command8_Click", 137
                Print #1, 1
  VBCP_Update 15, "Command8_Click", 138
                Print #1, Txs(2, J)
  VBCP_Update 15, "Command8_Click", 139
                Print #1, 0
  VBCP_Update 15, "Command8_Click", 140
            Case 4
  VBCP_Update 15, "Command8_Click", 141
                Print #1, 0
  VBCP_Update 15, "Command8_Click", 142
                Print #1, 0
  VBCP_Update 15, "Command8_Click", 143
                Print #1, 0
  VBCP_Update 15, "Command8_Click", 144
            Case 3
  VBCP_Update 15, "Command8_Click", 145
                Print #1, 4
  VBCP_Update 15, "Command8_Click", 146
                Print #1, Txs(2, J)
  VBCP_Update 15, "Command8_Click", 147
                Print #1, Txs(3, J)
  VBCP_Update 15, "Command8_Click", 148
        End Select
        If Vxs(1, J) = 0 And VBCP_UpdateIf(15, "Command8_Click", 149) Then Print #1, 1 Else Print #1, 0
    Next J
    For J = 1 To Ndyd
  VBCP_Update 15, "Command8_Click", 150
        For I = 1 To 2
  VBCP_Update 15, "Command8_Click", 151
            Print #1, Yd(I, J)
  VBCP_Update 15, "Command8_Click", 152
        Next I
        Select Case Tyd(1, J)
            Case 0
  VBCP_Update 15, "Command8_Click", 153
                Print #1, 1
  VBCP_Update 15, "Command8_Click", 154
                Print #1, 0
  VBCP_Update 15, "Command8_Click", 155
                Print #1, 0
  VBCP_Update 15, "Command8_Click", 156
            Case 1
  VBCP_Update 15, "Command8_Click", 157
                Print #1, 0
  VBCP_Update 15, "Command8_Click", 158
                Print #1, 0
  VBCP_Update 15, "Command8_Click", 159
                Print #1, 0
  VBCP_Update 15, "Command8_Click", 160
            Case 2
  VBCP_Update 15, "Command8_Click", 161
                Print #1, 1
  VBCP_Update 15, "Command8_Click", 162
                Print #1, Tyd(2, J)
  VBCP_Update 15, "Command8_Click", 163
                Print #1, 0
  VBCP_Update 15, "Command8_Click", 164
            Case 4
  VBCP_Update 15, "Command8_Click", 165
                Print #1, 0
  VBCP_Update 15, "Command8_Click", 166
                Print #1, 0
  VBCP_Update 15, "Command8_Click", 167
                Print #1, 0
  VBCP_Update 15, "Command8_Click", 168
            Case 3
  VBCP_Update 15, "Command8_Click", 169
                Print #1, 4
  VBCP_Update 15, "Command8_Click", 170
                Print #1, Tyd(2, J)
  VBCP_Update 15, "Command8_Click", 171
                Print #1, Tyd(3, J)
  VBCP_Update 15, "Command8_Click", 172
        End Select
        If Uyd(1, J) = 0 And VBCP_UpdateIf(15, "Command8_Click", 173) Then Print #1, 1 Else Print #1, 0
    Next J
Close #1
  VBCP_Update 15, "Command8_Click", 174

id = Shell("Igs.exe", vbNormalFocus)
  VBCP_Update 15, "Command8_Click", 175


End Sub
Private Sub Command9_Click()
  VBCP_Update 15, "Command9_Click", 1
q = Shell("pre", vbNormalFocus)
  VBCP_Update 15, "Command9_Click", 2
End
End Sub


Private Sub Form_Activate()
  VBCP_Update 15, "Form_Activate", 1
Label1 = "   Xl = " & Xl & "    " & Chr$(13)
  VBCP_Update 15, "Form_Activate", 2
Label1 = Label1 & "    Yl = " & Yl & "    " & Chr$(13)
  VBCP_Update 15, "Form_Activate", 3
If Mode = 2 And VBCP_UpdateIf(15, "Form_Activate", 4) Then Label1 = Label1 & "    R(1) = " & R1 & "    " & Chr$(13)
Label1 = Label1 & "    l1 = " & L1 & "    " & Chr$(13)
  VBCP_Update 15, "Form_Activate", 5
Label1 = Label1 & "    M1 = " & M1 & "    "
  VBCP_Update 15, "Form_Activate", 6

End Sub

Private Sub Form_Load()
  VBCP_Update 15, "Form_Load", 1
    For kk = 0 To 3
  VBCP_Update 15, "Form_Load", 2
        Pro(kk) = 0.1
  VBCP_Update 15, "Form_Load", 3
    Next kk
    
    CEscala
  VBCP_Update 15, "Form_Load", 4
    DiDominio
  VBCP_Update 15, "Form_Load", 5
    
    
End Sub

Private Sub Form_Unload(Cancel As Integer)
  VBCP_Update 15, "Form_Unload", 1
End
End Sub



