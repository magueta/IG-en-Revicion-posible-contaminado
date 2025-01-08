VERSION 5.00
Begin VB.Form FTfE 
   BorderStyle     =   1  'Fixed Single
   ClientHeight    =   3480
   ClientLeft      =   1380
   ClientTop       =   1470
   ClientWidth     =   5820
   ControlBox      =   0   'False
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   3480
   ScaleWidth      =   5820
   StartUpPosition =   2  'CenterScreen
   Begin VB.Frame Frame4 
      Caption         =   "Bloques"
      Height          =   1455
      Left            =   120
      TabIndex        =   2
      Top             =   840
      Width           =   2055
      Begin VB.TextBox Text2 
         Height          =   315
         Index           =   0
         Left            =   600
         TabIndex        =   5
         Top             =   600
         Width           =   735
      End
      Begin VB.TextBox Text2 
         Height          =   315
         Index           =   1
         Left            =   600
         TabIndex        =   4
         Top             =   1020
         Width           =   735
      End
      Begin VB.ComboBox Combo1 
         Height          =   315
         Left            =   180
         Style           =   2  'Dropdown List
         TabIndex        =   3
         Top             =   240
         Width           =   1755
      End
      Begin VB.Label Label2 
         Caption         =   "Sc"
         Height          =   255
         Index           =   0
         Left            =   180
         TabIndex        =   7
         Top             =   660
         Width           =   255
      End
      Begin VB.Label Label2 
         Caption         =   "Sp  "
         Height          =   255
         Index           =   1
         Left            =   180
         TabIndex        =   6
         Top             =   1020
         Width           =   375
      End
   End
   Begin VB.PictureBox Picture1 
      BackColor       =   &H00000000&
      ForeColor       =   &H00FFFFFF&
      Height          =   3195
      Left            =   2400
      ScaleHeight     =   3135
      ScaleWidth      =   3315
      TabIndex        =   1
      Top             =   120
      Width           =   3375
   End
   Begin VB.CommandButton Command1 
      Caption         =   "Seguir"
      Height          =   495
      Left            =   180
      TabIndex        =   0
      Top             =   2580
      Width           =   1215
   End
End
Attribute VB_Name = "FTfE"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False

Private Sub Combo1_Click()
  VBCP_Update 9, "Combo1_Click", 1
    For I = 0 To 1
  VBCP_Update 9, "Combo1_Click", 2
        Text2(I) = TFE(I + 1, Combo1.ListIndex + 1)
  VBCP_Update 9, "Combo1_Click", 3
    Next I
    DiMalla
  VBCP_Update 9, "Combo1_Click", 4
    J = (Combo1.ListIndex) \ Ndx + 1
  VBCP_Update 9, "Combo1_Click", 5
    I = Combo1.ListIndex + 1 - (J - 1) * Ndx
  VBCP_Update 9, "Combo1_Click", 6
    Picture1.Line (XU(LL(I - 1)), YV(MM(J - 1)))-(XU(LL(I)), YV(MM(J))), vbWhite, B
  VBCP_Update 9, "Combo1_Click", 7

End Sub

Private Sub Command1_Click()
  VBCP_Update 9, "Command1_Click", 1
Me.Hide
  VBCP_Update 9, "Command1_Click", 2
End Sub


Private Sub Form_Activate()
  VBCP_Update 9, "Form_Activate", 1
Combo1.Locked = False
  VBCP_Update 9, "Form_Activate", 2
CEscala
  VBCP_Update 9, "Form_Activate", 3
DiMalla
  VBCP_Update 9, "Form_Activate", 4
 
    Combo1.Clear
  VBCP_Update 9, "Form_Activate", 5
    Cont = 0
  VBCP_Update 9, "Form_Activate", 6
    For J = 1 To Ndy
  VBCP_Update 9, "Form_Activate", 7
    For I = 1 To Ndx
  VBCP_Update 9, "Form_Activate", 8
        Cont = Cont + 1
  VBCP_Update 9, "Form_Activate", 9
        Combo1.AddItem ("Bloque Nº " & Cont)
  VBCP_Update 9, "Form_Activate", 10
    Next I
    Next J
    Combo1.ListIndex = 0
  VBCP_Update 9, "Form_Activate", 11
End Sub

Sub CEscala()
  VBCP_Update 9, "CEscala", 1
    Mediaescala = (EscalaXY * 1.1) / 2
  VBCP_Update 9, "CEscala", 2
    With Picture1
        .ScaleHeight = -2 * Mediaescala
  VBCP_Update 9, "CEscala", 3
        .ScaleWidth = 2 * Mediaescala
  VBCP_Update 9, "CEscala", 4
        .ScaleLeft = -Mediaescala + Xl / 2
  VBCP_Update 9, "CEscala", 5
        .ScaleTop = Yl / 2 + Mediaescala
  VBCP_Update 9, "CEscala", 6
    End With
End Sub
Sub DiMalla()
  VBCP_Update 9, "DiMalla", 1
    Picture1.Cls
  VBCP_Update 9, "DiMalla", 2
    'For I = 1 To L1
    '     For J = 1 To M1
    '         Picture1.PSet (X(I), Y(J)), QBColor(12)
    '     Next J
    ' Next I
     For I = 2 To L2
  VBCP_Update 9, "DiMalla", 3
         For J = 2 To M2
  VBCP_Update 9, "DiMalla", 4
             Picture1.Line (XU(I), YV(J))-(XU(I), YV(J + 1)), QBColor(8)
  VBCP_Update 9, "DiMalla", 5
             Picture1.Line (XU(I), YV(J))-(XU(I + 1), YV(J)), QBColor(8)
  VBCP_Update 9, "DiMalla", 6
         Next J
     Next I
    Picture1.Line (XU(L1), YV(M1))-(XU(L1), YV(2)), QBColor(8)
  VBCP_Update 9, "DiMalla", 7
    Picture1.Line (XU(2), YV(M1))-(XU(L1), YV(M1)), QBColor(8)
  VBCP_Update 9, "DiMalla", 8
    If Ns > 0 And VBCP_UpdateIf(9, "DiMalla", 9) Then
        For K = 1 To Ns
  VBCP_Update 9, "DiMalla", 10
            Picture1.Line (S(0, K), S(1, K))-(S(2, K), S(3, K)), vbMagenta, B
  VBCP_Update 9, "DiMalla", 11
        Next K
    End If
End Sub


Private Sub Text2_Change(Index As Integer)
  VBCP_Update 9, "Text2_Change", 1
 TFE(Index + 1, Combo1.ListIndex + 1) = Val(Text2(Index))
  VBCP_Update 9, "Text2_Change", 2
End Sub



