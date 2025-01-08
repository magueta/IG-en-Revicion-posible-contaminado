VERSION 5.00
Begin VB.Form FMalla 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Mallado"
   ClientHeight    =   4365
   ClientLeft      =   1335
   ClientTop       =   1395
   ClientWidth     =   6360
   ClipControls    =   0   'False
   ControlBox      =   0   'False
   LinkTopic       =   "Form1"
   LockControls    =   -1  'True
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   4365
   ScaleWidth      =   6360
   StartUpPosition =   2  'CenterScreen
   Begin VB.Frame Frame1 
      Caption         =   "Configuración de bloques"
      Height          =   4155
      Left            =   60
      TabIndex        =   2
      Top             =   60
      Width           =   3135
      Begin VB.Frame Frame2 
         Caption         =   "Tipo de malla"
         Height          =   2145
         Left            =   60
         TabIndex        =   8
         Top             =   1920
         Width           =   3015
         Begin VB.TextBox Text3 
            Height          =   285
            Left            =   1980
            TabIndex        =   14
            Top             =   1680
            Width           =   795
         End
         Begin VB.OptionButton Option7 
            Caption         =   "Agrupados a la derecha"
            Height          =   255
            Left            =   60
            TabIndex        =   13
            Top             =   1200
            Width           =   2715
         End
         Begin VB.OptionButton Option6 
            Caption         =   "Agrupados a la izquierda"
            Height          =   315
            Left            =   60
            TabIndex        =   12
            Top             =   915
            Width           =   2715
         End
         Begin VB.OptionButton Option5 
            Caption         =   "Agrupados al centro"
            Height          =   255
            Left            =   60
            TabIndex        =   11
            Top             =   690
            Width           =   2715
         End
         Begin VB.OptionButton Option4 
            Caption         =   "Agrupados a los extremos"
            Height          =   255
            Left            =   60
            TabIndex        =   10
            Top             =   465
            Width           =   2715
         End
         Begin VB.OptionButton Option3 
            Caption         =   "Volumenes de control uniforme"
            Height          =   255
            Left            =   60
            TabIndex        =   9
            Top             =   240
            Value           =   -1  'True
            Width           =   2715
         End
         Begin VB.Label Label2 
            Alignment       =   2  'Center
            Caption         =   "Densidad de V.C. en la dirección escogida"
            Height          =   435
            Left            =   120
            TabIndex        =   16
            Top             =   1620
            Width           =   1755
         End
      End
      Begin VB.TextBox Text2 
         Height          =   285
         Left            =   1560
         TabIndex        =   7
         Top             =   1380
         Width           =   855
      End
      Begin VB.TextBox Text1 
         Height          =   285
         Left            =   240
         TabIndex        =   6
         Top             =   900
         Width           =   1635
      End
      Begin VB.VScrollBar VScroll1 
         Height          =   255
         Left            =   1860
         TabIndex        =   5
         Top             =   900
         Value           =   1
         Width           =   195
      End
      Begin VB.OptionButton Option2 
         Caption         =   "Bloques en la dirreción y"
         Height          =   195
         Left            =   480
         TabIndex        =   4
         Top             =   540
         Width           =   2355
      End
      Begin VB.OptionButton Option1 
         Caption         =   "Bloques en la dirreción x"
         Height          =   195
         Left            =   480
         TabIndex        =   3
         Top             =   300
         Value           =   -1  'True
         Width           =   2355
      End
      Begin VB.Label Label1 
         Alignment       =   2  'Center
         Caption         =   "Numeros de V.C. del bloque"
         Height          =   375
         Left            =   180
         TabIndex        =   15
         Top             =   1320
         Width           =   1335
      End
   End
   Begin VB.PictureBox Picture1 
      BackColor       =   &H00000000&
      ForeColor       =   &H00FFFFFF&
      Height          =   2835
      Left            =   3360
      ScaleHeight     =   2775
      ScaleWidth      =   2775
      TabIndex        =   1
      Top             =   120
      Width           =   2835
   End
   Begin VB.CommandButton Command1 
      Caption         =   "Seguir"
      Height          =   315
      Left            =   4800
      TabIndex        =   0
      Top             =   3600
      Width           =   795
   End
End
Attribute VB_Name = "FMalla"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False

Sub CEscala()
  VBCP_Update 7, "CEscala", 1
    Mediaescala = (EscalaXY * 1.1) / 2
  VBCP_Update 7, "CEscala", 2
    With Picture1
        .ScaleHeight = -2 * Mediaescala
  VBCP_Update 7, "CEscala", 3
        .ScaleWidth = 2 * Mediaescala
  VBCP_Update 7, "CEscala", 4
        .ScaleLeft = -Mediaescala + Xl / 2
  VBCP_Update 7, "CEscala", 5
        .ScaleTop = Yl / 2 + Mediaescala
  VBCP_Update 7, "CEscala", 6
    End With
End Sub

Private Sub Command1_Click()
  VBCP_Update 7, "Command1_Click", 1
    M3 = 0
  VBCP_Update 7, "Command1_Click", 2
    L3 = 0
  VBCP_Update 7, "Command1_Click", 3
    LL(1) = L(1)
  VBCP_Update 7, "Command1_Click", 4
    MM(1) = M(1)
  VBCP_Update 7, "Command1_Click", 5
    LL(0) = 2
  VBCP_Update 7, "Command1_Click", 6
    MM(0) = 2
  VBCP_Update 7, "Command1_Click", 7
    For K = 1 To Ndx
  VBCP_Update 7, "Command1_Click", 8
        L3 = L3 + L(K)
  VBCP_Update 7, "Command1_Click", 9
    Next K
    
    For K = 1 To Ndy
  VBCP_Update 7, "Command1_Click", 10
        M3 = M3 + M(K)
  VBCP_Update 7, "Command1_Click", 11
    Next K
    
    For K = 1 To Ndx
  VBCP_Update 7, "Command1_Click", 12
        LL(K) = LL(K - 1) + L(K)
  VBCP_Update 7, "Command1_Click", 13
    Next K
    
    For K = 1 To Ndy
  VBCP_Update 7, "Command1_Click", 14
        MM(K) = MM(K - 1) + M(K)
  VBCP_Update 7, "Command1_Click", 15
    Next K
    
    L2 = L3 + 1
  VBCP_Update 7, "Command1_Click", 16
    M2 = M3 + 1
  VBCP_Update 7, "Command1_Click", 17
    L1 = L2 + 1
  VBCP_Update 7, "Command1_Click", 18
    M1 = M2 + 1
  VBCP_Update 7, "Command1_Click", 19
    
    ReDim Preserve XU(L1), YV(M1), X(L1), Y(M1), XDif(L1), YDif(M1), XCV(L1), YCV(M1)
  VBCP_Update 7, "Command1_Click", 20
    
    For I = 1 To Ndx
  VBCP_Update 7, "Command1_Click", 21
        Call Expone(True, I, PowX(I), Tmx(I))
  VBCP_Update 7, "Command1_Click", 22
    Next I
    
    For I = 1 To Ndy
  VBCP_Update 7, "Command1_Click", 23
        Call Expone(False, I, PowY(I), Tmy(I))
  VBCP_Update 7, "Command1_Click", 24
    Next I
    
    Call Cadicio
  VBCP_Update 7, "Command1_Click", 25
    Me.Hide
  VBCP_Update 7, "Command1_Click", 26
End Sub

Private Sub Form_Activate()
  VBCP_Update 7, "Form_Activate", 1
    CEscala
  VBCP_Update 7, "Form_Activate", 2
    ReDim Preserve Tmx(Ndx), Tmy(Ndy), L(Ndx), M(Ndy), PowX(Ndx), _
    PowY(Ndy), LL(Ndx), MM(Ndy)
  VBCP_Update 7, "Form_Activate", 3
    ReDim BloqX(Ndx), BloqY(Ndy)
    For K = 1 To Ndx
  VBCP_Update 7, "Form_Activate", 4
        BloqX(K) = "Bloque X " & Str(K)
  VBCP_Update 7, "Form_Activate", 5
    Next K
        For K = 1 To Ndy
  VBCP_Update 7, "Form_Activate", 6
        BloqY(K) = "Bloque Y " & Str(K)
  VBCP_Update 7, "Form_Activate", 7
    Next K
    Picture1.Cls
  VBCP_Update 7, "Form_Activate", 8
    VScroll1.Max = 1
  VBCP_Update 7, "Form_Activate", 9
    VScroll1.Min = Ndx
  VBCP_Update 7, "Form_Activate", 10
    VScroll1 = 1
  VBCP_Update 7, "Form_Activate", 11
    Old = Picture1.FillStyle
  VBCP_Update 7, "Form_Activate", 12
    OldC = Picture1.FillColor
  VBCP_Update 7, "Form_Activate", 13
    Picture1.FillStyle = 3
  VBCP_Update 7, "Form_Activate", 14
    Picture1.FillColor = QBColor(10)
  VBCP_Update 7, "Form_Activate", 15
    Text1 = BloqX(VScroll1)
  VBCP_Update 7, "Form_Activate", 16
    Picture1.Line (Dx(1, VScroll1) + Dx(0, VScroll1), 0)-(Dx(0, VScroll1), Yl), , B
  VBCP_Update 7, "Form_Activate", 17
    Picture1.FillStyle = Old
  VBCP_Update 7, "Form_Activate", 18
    Picture1.FillColor = OldC
  VBCP_Update 7, "Form_Activate", 19
    Picture1.Line (0, 0)-(Xl, Yl), , B
  VBCP_Update 7, "Form_Activate", 20
    Option1 = True
  VBCP_Update 7, "Form_Activate", 21
    If Ns > 0 And VBCP_UpdateIf(7, "Form_Activate", 22) Then
        For K = 1 To Ns
  VBCP_Update 7, "Form_Activate", 23
            Picture1.Line (S(0, K), S(1, K))-(S(2, K), S(3, K)), , B
  VBCP_Update 7, "Form_Activate", 24
            Picture1.CurrentX = S(0, K) / 2 + S(2, K) / 2
  VBCP_Update 7, "Form_Activate", 25
            Picture1.CurrentY = S(1, K) / 2 + S(3, K) / 2
  VBCP_Update 7, "Form_Activate", 26
        Next K
    End If
  
    
    
End Sub

Private Sub Option1_Click()
  VBCP_Update 7, "Option1_Click", 1
Picture1.Cls
  VBCP_Update 7, "Option1_Click", 2
VScroll1.Max = 1
  VBCP_Update 7, "Option1_Click", 3
VScroll1.Min = Ndx
  VBCP_Update 7, "Option1_Click", 4
VScroll1 = 1
  VBCP_Update 7, "Option1_Click", 5
Option3.Caption = "Volumenes de control uniforme"
  VBCP_Update 7, "Option1_Click", 6
Option4.Caption = "Agrupados a los extremos"
  VBCP_Update 7, "Option1_Click", 7
Option5.Caption = "Agrupados al centro"
  VBCP_Update 7, "Option1_Click", 8
Option6.Caption = "Agrupados a la izquierda"
  VBCP_Update 7, "Option1_Click", 9
Option7.Caption = "Agrupados a la derecha"
  VBCP_Update 7, "Option1_Click", 10
Old = Picture1.FillStyle
  VBCP_Update 7, "Option1_Click", 11
OldC = Picture1.FillColor
  VBCP_Update 7, "Option1_Click", 12
Picture1.FillStyle = 3
  VBCP_Update 7, "Option1_Click", 13
Picture1.FillColor = QBColor(10)
  VBCP_Update 7, "Option1_Click", 14
Text1 = BloqX(VScroll1)
  VBCP_Update 7, "Option1_Click", 15
Text2 = L(VScroll1)
  VBCP_Update 7, "Option1_Click", 16
Text3 = PowX(VScroll1)
  VBCP_Update 7, "Option1_Click", 17
Select Case Tmx(VScroll1)
    Case 1
  VBCP_Update 7, "Option1_Click", 18
        Option3 = True
  VBCP_Update 7, "Option1_Click", 19
    Case 2
  VBCP_Update 7, "Option1_Click", 20
        Option4 = True
  VBCP_Update 7, "Option1_Click", 21
    Case 3
  VBCP_Update 7, "Option1_Click", 22
        Option5 = True
  VBCP_Update 7, "Option1_Click", 23
    Case 4
  VBCP_Update 7, "Option1_Click", 24
        Option6 = True
  VBCP_Update 7, "Option1_Click", 25
    Case 5
  VBCP_Update 7, "Option1_Click", 26
        Option7 = True
  VBCP_Update 7, "Option1_Click", 27
    Case Else
  VBCP_Update 7, "Option1_Click", 28
        Tmx(VScroll1) = 1
  VBCP_Update 7, "Option1_Click", 29
        Option3 = True
  VBCP_Update 7, "Option1_Click", 30
        PowX(VScroll1) = 1
  VBCP_Update 7, "Option1_Click", 31
End Select
Picture1.Line (Dx(1, VScroll1) + Dx(0, VScroll1), 0)-(Dx(0, VScroll1), Yl), , B
  VBCP_Update 7, "Option1_Click", 32
Picture1.FillStyle = Old
  VBCP_Update 7, "Option1_Click", 33
Picture1.FillColor = OldC
  VBCP_Update 7, "Option1_Click", 34
Picture1.Line (0, 0)-(Xl, Yl), , B
  VBCP_Update 7, "Option1_Click", 35
     If Ns > 0 And VBCP_UpdateIf(7, "Option1_Click", 36) Then
        For K = 1 To Ns
  VBCP_Update 7, "Option1_Click", 37
            Picture1.Line (S(0, K), S(1, K))-(S(2, K), S(3, K)), , B
  VBCP_Update 7, "Option1_Click", 38
            Picture1.CurrentX = S(0, K) / 2 + S(2, K) / 2
  VBCP_Update 7, "Option1_Click", 39
            Picture1.CurrentY = S(1, K) / 2 + S(3, K) / 2
  VBCP_Update 7, "Option1_Click", 40
        Next K
    End If
End Sub


Private Sub Option2_Click()
  VBCP_Update 7, "Option2_Click", 1
Picture1.Cls
  VBCP_Update 7, "Option2_Click", 2
VScroll1.Max = 1
  VBCP_Update 7, "Option2_Click", 3
VScroll1.Min = Ndy
  VBCP_Update 7, "Option2_Click", 4
VScroll1 = 1
  VBCP_Update 7, "Option2_Click", 5
Option3.Caption = "Volumenes de control uniforme"
  VBCP_Update 7, "Option2_Click", 6
Option4.Caption = "Agrupados a los extremos"
  VBCP_Update 7, "Option2_Click", 7
Option5.Caption = "Agrupados al centro"
  VBCP_Update 7, "Option2_Click", 8
Option6.Caption = "Agrupados abajo"
  VBCP_Update 7, "Option2_Click", 9
Option7.Caption = "Agrupados ariba"
  VBCP_Update 7, "Option2_Click", 10
Old = Picture1.FillStyle
  VBCP_Update 7, "Option2_Click", 11
OldC = Picture1.FillColor
  VBCP_Update 7, "Option2_Click", 12
Picture1.FillStyle = 2
  VBCP_Update 7, "Option2_Click", 13
Picture1.FillColor = QBColor(10)
  VBCP_Update 7, "Option2_Click", 14
Text1 = BloqY(VScroll1)
  VBCP_Update 7, "Option2_Click", 15
Text2 = M(VScroll1)
  VBCP_Update 7, "Option2_Click", 16
Text3 = PowY(VScroll1)
  VBCP_Update 7, "Option2_Click", 17
Picture1.Line (0, Dy(1, VScroll1) + Dy(0, VScroll1))-(Xl, Dy(0, VScroll1)), , B
  VBCP_Update 7, "Option2_Click", 18
Picture1.FillStyle = Old
  VBCP_Update 7, "Option2_Click", 19
Picture1.FillColor = OldC
  VBCP_Update 7, "Option2_Click", 20
Picture1.Line (0, 0)-(Xl, Yl), , B
  VBCP_Update 7, "Option2_Click", 21
Select Case Tmy(VScroll1)
    Case 1
  VBCP_Update 7, "Option2_Click", 22
        Option3 = True
  VBCP_Update 7, "Option2_Click", 23
    Case 2
  VBCP_Update 7, "Option2_Click", 24
        Option4 = True
  VBCP_Update 7, "Option2_Click", 25
    Case 3
  VBCP_Update 7, "Option2_Click", 26
        Option5 = True
  VBCP_Update 7, "Option2_Click", 27
    Case 4
  VBCP_Update 7, "Option2_Click", 28
        Option6 = True
  VBCP_Update 7, "Option2_Click", 29
    Case 5
  VBCP_Update 7, "Option2_Click", 30
        Option7 = True
  VBCP_Update 7, "Option2_Click", 31
    Case Else
  VBCP_Update 7, "Option2_Click", 32
        Tmy(VScroll1) = 1
  VBCP_Update 7, "Option2_Click", 33
        Option3 = True
  VBCP_Update 7, "Option2_Click", 34
        PowY(VScroll1) = 1
  VBCP_Update 7, "Option2_Click", 35
End Select

If Ns > 0 And VBCP_UpdateIf(7, "Option2_Click", 36) Then
    For K = 1 To Ns
  VBCP_Update 7, "Option2_Click", 37
        Picture1.Line (S(0, K), S(1, K))-(S(2, K), S(3, K)), , B
  VBCP_Update 7, "Option2_Click", 38
        Picture1.CurrentX = S(0, K) / 2 + S(2, K) / 2
  VBCP_Update 7, "Option2_Click", 39
        Picture1.CurrentY = S(1, K) / 2 + S(3, K) / 2
  VBCP_Update 7, "Option2_Click", 40
    Next K
End If

End Sub


Private Sub Option3_Click()
  VBCP_Update 7, "Option3_Click", 1
If Option1 And VBCP_UpdateIf(7, "Option3_Click", 2) Then
    Tmx(VScroll1) = 1
  VBCP_Update 7, "Option3_Click", 3
Else
    Tmy(VScroll1) = 1
  VBCP_Update 7, "Option3_Click", 4
End If
End Sub

Private Sub Option4_Click()
  VBCP_Update 7, "Option4_Click", 1
If Option1 And VBCP_UpdateIf(7, "Option4_Click", 2) Then
    Tmx(VScroll1) = 2
  VBCP_Update 7, "Option4_Click", 3
Else
    Tmy(VScroll1) = 2
  VBCP_Update 7, "Option4_Click", 4
End If

End Sub


Private Sub Option5_Click()
  VBCP_Update 7, "Option5_Click", 1
If Option1 And VBCP_UpdateIf(7, "Option5_Click", 2) Then
    Tmx(VScroll1) = 3
  VBCP_Update 7, "Option5_Click", 3
Else
    Tmy(VScroll1) = 3
  VBCP_Update 7, "Option5_Click", 4
End If

End Sub


Private Sub Option6_Click()
  VBCP_Update 7, "Option6_Click", 1
If Option1 And VBCP_UpdateIf(7, "Option6_Click", 2) Then
    Tmx(VScroll1) = 4
  VBCP_Update 7, "Option6_Click", 3
Else
    Tmy(VScroll1) = 4
  VBCP_Update 7, "Option6_Click", 4
End If

End Sub


Private Sub Option7_Click()
  VBCP_Update 7, "Option7_Click", 1
If Option1 And VBCP_UpdateIf(7, "Option7_Click", 2) Then
    Tmx(VScroll1) = 5
  VBCP_Update 7, "Option7_Click", 3
Else
    Tmy(VScroll1) = 5
  VBCP_Update 7, "Option7_Click", 4
End If

End Sub


Private Sub Text2_Change()
  VBCP_Update 7, "Text2_Change", 1
If Option1 And VBCP_UpdateIf(7, "Text2_Change", 2) Then
    L(VScroll1) = Val(Text2)
  VBCP_Update 7, "Text2_Change", 3
Else
    M(VScroll1) = Val(Text2)
  VBCP_Update 7, "Text2_Change", 4
End If

End Sub

Private Sub Text3_Change()
  VBCP_Update 7, "Text3_Change", 1
If Option1 And VBCP_UpdateIf(7, "Text3_Change", 2) Then
    PowX(VScroll1) = Val(Text3)
  VBCP_Update 7, "Text3_Change", 3
Else
    PowY(VScroll1) = Val(Text3)
  VBCP_Update 7, "Text3_Change", 4
End If
End Sub


Private Sub VScroll1_Change()
  VBCP_Update 7, "VScroll1_Change", 1
Picture1.Cls
  VBCP_Update 7, "VScroll1_Change", 2
Old = Picture1.FillStyle
  VBCP_Update 7, "VScroll1_Change", 3
OldC = Picture1.FillColor
  VBCP_Update 7, "VScroll1_Change", 4
Picture1.FillColor = QBColor(10)
  VBCP_Update 7, "VScroll1_Change", 5
Select Case Option1.Value
    Case True
  VBCP_Update 7, "VScroll1_Change", 6
        Text1 = BloqX(VScroll1)
  VBCP_Update 7, "VScroll1_Change", 7
        Text2 = L(VScroll1)
  VBCP_Update 7, "VScroll1_Change", 8
        Select Case Tmx(VScroll1)
            Case 1
  VBCP_Update 7, "VScroll1_Change", 9
                Option3 = True
  VBCP_Update 7, "VScroll1_Change", 10
            Case 2
  VBCP_Update 7, "VScroll1_Change", 11
                Option4 = True
  VBCP_Update 7, "VScroll1_Change", 12
            Case 3
  VBCP_Update 7, "VScroll1_Change", 13
                Option5 = True
  VBCP_Update 7, "VScroll1_Change", 14
            Case 4
  VBCP_Update 7, "VScroll1_Change", 15
                Option6 = True
  VBCP_Update 7, "VScroll1_Change", 16
            Case 5
  VBCP_Update 7, "VScroll1_Change", 17
                Option7 = True
  VBCP_Update 7, "VScroll1_Change", 18
            Case Else
  VBCP_Update 7, "VScroll1_Change", 19
                Tmx(VScroll1) = 1
  VBCP_Update 7, "VScroll1_Change", 20
                Option3 = True
  VBCP_Update 7, "VScroll1_Change", 21
                PowX(VScroll1) = 1
  VBCP_Update 7, "VScroll1_Change", 22
        End Select
        Text3 = PowX(VScroll1)
  VBCP_Update 7, "VScroll1_Change", 23
        Picture1.FillStyle = 3
  VBCP_Update 7, "VScroll1_Change", 24
        Picture1.Line (Dx(1, VScroll1) + Dx(0, VScroll1), 0)-(Dx(0, VScroll1), Yl), , B
  VBCP_Update 7, "VScroll1_Change", 25
        Picture1.FillStyle = Old
  VBCP_Update 7, "VScroll1_Change", 26
        Picture1.FillColor = OldC
  VBCP_Update 7, "VScroll1_Change", 27
    Case False
  VBCP_Update 7, "VScroll1_Change", 28
        Text1 = BloqY(VScroll1)
  VBCP_Update 7, "VScroll1_Change", 29
        Picture1.FillStyle = 2
  VBCP_Update 7, "VScroll1_Change", 30
        Picture1.Line (0, Dy(1, VScroll1) + Dy(0, VScroll1))-(Xl, Dy(0, VScroll1)), , B
  VBCP_Update 7, "VScroll1_Change", 31
        Picture1.FillStyle = Old
  VBCP_Update 7, "VScroll1_Change", 32
        Picture1.FillColor = OldC
  VBCP_Update 7, "VScroll1_Change", 33
        Text2 = M(VScroll1)
  VBCP_Update 7, "VScroll1_Change", 34
        Select Case Tmy(VScroll1)
            Case 1
  VBCP_Update 7, "VScroll1_Change", 35
                Option3 = True
  VBCP_Update 7, "VScroll1_Change", 36
            Case 2
  VBCP_Update 7, "VScroll1_Change", 37
                Option4 = True
  VBCP_Update 7, "VScroll1_Change", 38
            Case 3
  VBCP_Update 7, "VScroll1_Change", 39
                Option5 = True
  VBCP_Update 7, "VScroll1_Change", 40
            Case 4
  VBCP_Update 7, "VScroll1_Change", 41
                Option6 = True
  VBCP_Update 7, "VScroll1_Change", 42
            Case 5
  VBCP_Update 7, "VScroll1_Change", 43
                Option7 = True
  VBCP_Update 7, "VScroll1_Change", 44
            Case Else
  VBCP_Update 7, "VScroll1_Change", 45
                Tmy(VScroll1) = 1
  VBCP_Update 7, "VScroll1_Change", 46
                Option3 = True
  VBCP_Update 7, "VScroll1_Change", 47
                PowY(VScroll1) = 1
  VBCP_Update 7, "VScroll1_Change", 48
        End Select
        Text3 = PowY(VScroll1)
  VBCP_Update 7, "VScroll1_Change", 49
End Select
 Picture1.Line (0, 0)-(Xl, Yl), , B
  VBCP_Update 7, "VScroll1_Change", 50
 If Ns > 0 And VBCP_UpdateIf(7, "VScroll1_Change", 51) Then
    For K = 1 To Ns
  VBCP_Update 7, "VScroll1_Change", 52
        
        Picture1.Line (S(0, K), S(1, K))-(S(2, K), S(3, K)), , B
  VBCP_Update 7, "VScroll1_Change", 53
        Picture1.CurrentX = S(0, K) / 2 + S(2, K) / 2
  VBCP_Update 7, "VScroll1_Change", 54
        Picture1.CurrentY = S(1, K) / 2 + S(3, K) / 2
  VBCP_Update 7, "VScroll1_Change", 55
    Next K
End If
    
End Sub



