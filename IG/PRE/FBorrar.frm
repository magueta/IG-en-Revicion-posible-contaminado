VERSION 5.00
Begin VB.Form FBorrar 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Editar Sólidos"
   ClientHeight    =   4935
   ClientLeft      =   540
   ClientTop       =   1050
   ClientWidth     =   8340
   ClipControls    =   0   'False
   ControlBox      =   0   'False
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   4935
   ScaleWidth      =   8340
   ShowInTaskbar   =   0   'False
   StartUpPosition =   2  'CenterScreen
   Begin VB.CommandButton Command1 
      Caption         =   "Cancelar"
      Height          =   495
      Left            =   4463
      TabIndex        =   20
      Top             =   4320
      Width           =   1215
   End
   Begin VB.CommandButton Command2 
      Caption         =   "Borrar"
      Height          =   495
      Left            =   2663
      TabIndex        =   19
      Top             =   4320
      Width           =   1215
   End
   Begin VB.VScrollBar VScroll1 
      Height          =   255
      Left            =   2760
      TabIndex        =   18
      Top             =   960
      Width           =   135
   End
   Begin VB.TextBox Text2 
      Height          =   285
      Left            =   480
      TabIndex        =   17
      Top             =   960
      Width           =   2295
   End
   Begin VB.TextBox Text1 
      Height          =   315
      Index           =   0
      Left            =   4260
      TabIndex        =   11
      Top             =   360
      Width           =   975
   End
   Begin VB.TextBox Text1 
      Height          =   315
      Index           =   1
      Left            =   4260
      TabIndex        =   10
      Top             =   780
      Width           =   975
   End
   Begin VB.TextBox Text1 
      Height          =   315
      Index           =   2
      Left            =   5820
      TabIndex        =   9
      Top             =   360
      Width           =   975
   End
   Begin VB.TextBox Text1 
      Height          =   315
      Index           =   3
      Left            =   5820
      TabIndex        =   8
      Top             =   780
      Width           =   975
   End
   Begin VB.Frame Frame1 
      Caption         =   "Propiedades"
      Height          =   1695
      Left            =   3600
      TabIndex        =   1
      Top             =   1800
      Width           =   3495
      Begin VB.TextBox Text1 
         Height          =   315
         Index           =   5
         Left            =   1800
         TabIndex        =   4
         Text            =   "1"
         Top             =   780
         Width           =   1215
      End
      Begin VB.TextBox Text1 
         Height          =   315
         Index           =   4
         Left            =   1800
         TabIndex        =   3
         Text            =   "1"
         Top             =   360
         Width           =   1215
      End
      Begin VB.TextBox Text1 
         Height          =   315
         Index           =   6
         Left            =   1800
         TabIndex        =   2
         Text            =   "1"
         Top             =   1200
         Width           =   1215
      End
      Begin VB.Label Label5 
         Caption         =   "Conductividad"
         Height          =   195
         Left            =   360
         TabIndex        =   7
         Top             =   420
         Width           =   1335
      End
      Begin VB.Label Label6 
         Caption         =   "Densidad"
         Height          =   195
         Left            =   360
         TabIndex        =   6
         Top             =   840
         Width           =   1335
      End
      Begin VB.Label Label8 
         Caption         =   "Cp"
         Height          =   195
         Left            =   360
         TabIndex        =   5
         Top             =   1260
         Width           =   1335
      End
   End
   Begin VB.PictureBox Picture1 
      Appearance      =   0  'Flat
      AutoRedraw      =   -1  'True
      BackColor       =   &H00000000&
      FillColor       =   &H00800000&
      ForeColor       =   &H00FFFFFF&
      Height          =   2175
      Left            =   480
      ScaleHeight     =   2145
      ScaleWidth      =   2145
      TabIndex        =   0
      Top             =   1440
      Width           =   2175
   End
   Begin VB.Label Label1 
      Caption         =   "X1"
      Height          =   195
      Left            =   3840
      TabIndex        =   16
      Top             =   420
      Width           =   1215
   End
   Begin VB.Label Label2 
      Caption         =   "Y1"
      Height          =   195
      Left            =   3840
      TabIndex        =   15
      Top             =   840
      Width           =   1215
   End
   Begin VB.Label Label3 
      Caption         =   "X2"
      Height          =   195
      Left            =   5400
      TabIndex        =   14
      Top             =   420
      Width           =   1215
   End
   Begin VB.Label Label4 
      Caption         =   "Y2"
      Height          =   195
      Left            =   5400
      TabIndex        =   13
      Top             =   840
      Width           =   1215
   End
   Begin VB.Label Label7 
      BorderStyle     =   1  'Fixed Single
      Caption         =   "Label7"
      Height          =   495
      Left            =   3600
      TabIndex        =   12
      Top             =   1200
      Width           =   3495
   End
End
Attribute VB_Name = "FBorrar"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False

Private Sub Command1_Click()
  VBCP_Update 4, "Command1_Click", 1
    Me.Hide
  VBCP_Update 4, "Command1_Click", 2
    Bl = False
  VBCP_Update 4, "Command1_Click", 3
End Sub

Private Sub Command2_Click()
  VBCP_Update 4, "Command2_Click", 1
If VScroll1 = 0 And VBCP_UpdateIf(4, "Command2_Click", 2) Then
    Mensa$ = "No hay  Solido seleccionado!!"
  VBCP_Update 4, "Command2_Click", 3
    re = MsgBox(Mensa$, vbOKOnly + vbApplicationModal + vbCritical, "Advertencia")
  VBCP_Update 4, "Command2_Click", 4
Else
    Mensa$ = "Estas seguro de Borrar el Solido Nº " & VScroll1 & " ?"
  VBCP_Update 4, "Command2_Click", 5
    re = MsgBox(Mensa$, vbYesNo + vbApplicationModal + vbQuestion, "Comfirmación")
  VBCP_Update 4, "Command2_Click", 6
    If re = vbYes And VBCP_UpdateIf(4, "Command2_Click", 7) Then
            For II = 1 To Ns - 1
  VBCP_Update 4, "Command2_Click", 8
                If II >= VScroll1 And VBCP_UpdateIf(4, "Command2_Click", 9) Then
                    For Ji = 0 To 6
  VBCP_Update 4, "Command2_Click", 10
                        S(Ji, II) = S(Ji, II + 1)
  VBCP_Update 4, "Command2_Click", 11
                    Next Ji
                End If
            Next II
            Ns = Ns - 1
  VBCP_Update 4, "Command2_Click", 12
            ReDim Preserve S(6, Ns)
  VBCP_Update 4, "Command2_Click", 13
            DiDominio
  VBCP_Update 4, "Command2_Click", 14
            DiSolido
  VBCP_Update 4, "Command2_Click", 15
            VScroll1.Max = Ns
  VBCP_Update 4, "Command2_Click", 16
            VScroll1.Min = 0
  VBCP_Update 4, "Command2_Click", 17
            VScroll1.Value = 0
  VBCP_Update 4, "Command2_Click", 18
        End If
    End If
End Sub

Private Sub Form_Activate()
  VBCP_Update 4, "Form_Activate", 1
    Command1.Caption = "Seguir"
  VBCP_Update 4, "Form_Activate", 2
    Label7 = "Xmax= " & Xl & Chr$(13) & " Ymax= " & Yl
  VBCP_Update 4, "Form_Activate", 3
    CEscala
  VBCP_Update 4, "Form_Activate", 4
    DiDominio
  VBCP_Update 4, "Form_Activate", 5
    DiSolido
  VBCP_Update 4, "Form_Activate", 6
    VScroll1.Max = Ns
  VBCP_Update 4, "Form_Activate", 7
    VScroll1.Min = 0
  VBCP_Update 4, "Form_Activate", 8
    VScroll1.Value = 0
  VBCP_Update 4, "Form_Activate", 9
    Text2 = "No hay Solido seleccionado"
  VBCP_Update 4, "Form_Activate", 10
    
End Sub

Sub DiSolido()
  VBCP_Update 4, "DiSolido", 1
    If Ns > 0 And VBCP_UpdateIf(4, "DiSolido", 2) Then
        For K = 1 To Ns
  VBCP_Update 4, "DiSolido", 3
            Picture1.Line (S(0, K), S(1, K))-(S(2, K), S(3, K)), (vbMagenta), B
  VBCP_Update 4, "DiSolido", 4
            Picture1.CurrentX = S(0, K) / 2 + S(2, K) / 2
  VBCP_Update 4, "DiSolido", 5
            Picture1.CurrentY = S(1, K) / 2 + S(3, K) / 2
  VBCP_Update 4, "DiSolido", 6
            Picture1.Print K
  VBCP_Update 4, "DiSolido", 7
        Next K
    End If
End Sub

Sub CEscala()
  VBCP_Update 4, "CEscala", 1
    EscalaXY = Xl
  VBCP_Update 4, "CEscala", 2
    If Yl > Xl And VBCP_UpdateIf(4, "CEscala", 3) Then
        EscalaXY = Yl
  VBCP_Update 4, "CEscala", 4
    End If
    Mediaescala = (EscalaXY * 1.1) / 2
  VBCP_Update 4, "CEscala", 5
    With Picture1
        .ScaleHeight = -2 * Mediaescala
  VBCP_Update 4, "CEscala", 6
        .ScaleWidth = 2 * Mediaescala
  VBCP_Update 4, "CEscala", 7
        .ScaleLeft = -Mediaescala + Xl / 2
  VBCP_Update 4, "CEscala", 8
        .ScaleTop = Yl / 2 + Mediaescala
  VBCP_Update 4, "CEscala", 9
    End With
End Sub

Sub DiDominio()
  VBCP_Update 4, "DiDominio", 1
    Picture1.Cls
  VBCP_Update 4, "DiDominio", 2
    Picture1.Line (0, 0)-(Xl, Yl), , B
  VBCP_Update 4, "DiDominio", 3
End Sub

Private Sub Text1_Change(Index As Integer)
  VBCP_Update 4, "Text1_Change", 1
If VScroll1 <> 0 And VBCP_UpdateIf(4, "Text1_Change", 2) Then
            S(Index, VScroll1) = Val(Text1(Index))
  VBCP_Update 4, "Text1_Change", 3
End If
End Sub

Private Sub VScroll1_Change()
  VBCP_Update 4, "VScroll1_Change", 1
If VScroll1 = 0 And VBCP_UpdateIf(4, "VScroll1_Change", 2) Then
    Text2 = "No hay Solido seleccionado"
  VBCP_Update 4, "VScroll1_Change", 3
    DiDominio
  VBCP_Update 4, "VScroll1_Change", 4
    DiSolido
  VBCP_Update 4, "VScroll1_Change", 5
    For K = 0 To 6
  VBCP_Update 4, "VScroll1_Change", 6
         Text1(K) = ""
  VBCP_Update 4, "VScroll1_Change", 7
    Next K

Else
    Text2 = "Solido Nº " & VScroll1
  VBCP_Update 4, "VScroll1_Change", 8
    DiDominio
  VBCP_Update 4, "VScroll1_Change", 9
    DiSolido
  VBCP_Update 4, "VScroll1_Change", 10

    For K = 0 To 6
  VBCP_Update 4, "VScroll1_Change", 11
         Text1(K) = S(K, VScroll1)
  VBCP_Update 4, "VScroll1_Change", 12
    Next K
    Picture1.Line (S(0, VScroll1), S(1, VScroll1))-(S(2, VScroll1), S(3, VScroll1)), vbYellow, BF
  VBCP_Update 4, "VScroll1_Change", 13
End If
End Sub


