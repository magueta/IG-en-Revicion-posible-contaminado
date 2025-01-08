VERSION 5.00
Begin VB.Form FV 
   BorderStyle     =   4  'Fixed ToolWindow
   Caption         =   "Ecuación de momento en Y"
   ClientHeight    =   6300
   ClientLeft      =   165
   ClientTop       =   375
   ClientWidth     =   8940
   LinkTopic       =   "Form1"
   LockControls    =   -1  'True
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   6300
   ScaleWidth      =   8940
   ShowInTaskbar   =   0   'False
   StartUpPosition =   2  'CenterScreen
   Begin VB.Frame Frame1 
      Caption         =   "Frontera"
      Enabled         =   0   'False
      Height          =   1215
      Left            =   360
      TabIndex        =   19
      Top             =   300
      Width           =   1755
      Begin VB.OptionButton Option1 
         Caption         =   "Derecha"
         Height          =   195
         Index           =   3
         Left            =   120
         TabIndex        =   23
         Top             =   960
         Width           =   1155
      End
      Begin VB.OptionButton Option1 
         Caption         =   "Superior"
         Height          =   195
         Index           =   2
         Left            =   120
         TabIndex        =   22
         Top             =   720
         Width           =   1155
      End
      Begin VB.OptionButton Option1 
         Caption         =   "Izquierda"
         Height          =   195
         Index           =   1
         Left            =   120
         TabIndex        =   21
         Top             =   480
         Width           =   1155
      End
      Begin VB.OptionButton Option1 
         Caption         =   "Inferior"
         Height          =   195
         Index           =   0
         Left            =   120
         TabIndex        =   20
         Top             =   240
         Width           =   1155
      End
   End
   Begin VB.Frame Frame2 
      Caption         =   "Ubicación"
      Enabled         =   0   'False
      Height          =   1335
      Left            =   360
      TabIndex        =   13
      Top             =   1560
      Width           =   1755
      Begin VB.TextBox Text1 
         Height          =   285
         Index           =   1
         Left            =   840
         TabIndex        =   16
         Top             =   960
         Width           =   615
      End
      Begin VB.TextBox Text1 
         Height          =   285
         Index           =   0
         Left            =   840
         TabIndex        =   15
         Top             =   600
         Width           =   615
      End
      Begin VB.ComboBox Combo1 
         Height          =   315
         Left            =   840
         TabIndex        =   14
         Text            =   "Combo1"
         Top             =   240
         Width           =   615
      End
      Begin VB.Label Label1 
         Caption         =   "Región Nº"
         Height          =   195
         Left            =   60
         TabIndex        =   24
         Top             =   300
         Width           =   735
      End
      Begin VB.Label Label2 
         Caption         =   "V.C.S."
         Height          =   195
         Index           =   1
         Left            =   300
         TabIndex        =   18
         Top             =   1020
         Width           =   435
      End
      Begin VB.Label Label2 
         Caption         =   "V.C.I."
         Height          =   195
         Index           =   0
         Left            =   300
         TabIndex        =   17
         Top             =   660
         Width           =   435
      End
   End
   Begin VB.Frame Frame3 
      Caption         =   "Tipo de Frontera"
      Enabled         =   0   'False
      Height          =   1935
      Left            =   360
      TabIndex        =   4
      Top             =   2940
      Width           =   1695
      Begin VB.OptionButton Option2 
         Caption         =   "Flujo t. Desarr."
         Height          =   195
         Index           =   0
         Left            =   180
         TabIndex        =   12
         Top             =   720
         Width           =   1455
      End
      Begin VB.OptionButton Option2 
         Caption         =   "Perfil Parabólico"
         Height          =   195
         Index           =   1
         Left            =   180
         TabIndex        =   11
         Top             =   480
         Width           =   1455
      End
      Begin VB.OptionButton Option2 
         Caption         =   "Constante"
         Height          =   195
         Index           =   2
         Left            =   180
         TabIndex        =   10
         Top             =   240
         Width           =   1095
      End
      Begin VB.Frame Frame4 
         Caption         =   "Valor"
         Height          =   915
         Left            =   60
         TabIndex        =   5
         Top             =   960
         Visible         =   0   'False
         Width           =   1575
         Begin VB.TextBox Text2 
            Height          =   285
            Index           =   1
            Left            =   720
            TabIndex        =   7
            Top             =   540
            Width           =   615
         End
         Begin VB.TextBox Text2 
            Height          =   285
            Index           =   0
            Left            =   720
            TabIndex        =   6
            Top             =   180
            Width           =   615
         End
         Begin VB.Label Label3 
            Caption         =   "h"
            Height          =   195
            Index           =   1
            Left            =   120
            TabIndex        =   9
            Top             =   600
            Width           =   735
         End
         Begin VB.Label Label3 
            Caption         =   "v"
            Height          =   195
            Index           =   0
            Left            =   120
            TabIndex        =   8
            Top             =   240
            Width           =   735
         End
      End
   End
   Begin VB.CheckBox Check1 
      Caption         =   "Activar"
      Height          =   195
      Left            =   60
      TabIndex        =   3
      Top             =   60
      Width           =   2535
   End
   Begin VB.CommandButton Command1 
      Caption         =   "Térnino Fuente"
      Enabled         =   0   'False
      Height          =   255
      Left            =   420
      TabIndex        =   2
      Top             =   5160
      Width           =   1575
   End
   Begin VB.CommandButton Command2 
      Caption         =   "Seguir"
      Height          =   255
      Left            =   420
      TabIndex        =   1
      Top             =   5700
      Width           =   1575
   End
   Begin VB.PictureBox Picture1 
      BackColor       =   &H00000000&
      ForeColor       =   &H00FFFFFF&
      Height          =   6135
      Left            =   2700
      ScaleHeight     =   6075
      ScaleWidth      =   6075
      TabIndex        =   0
      Top             =   60
      Width           =   6135
   End
End
Attribute VB_Name = "FV"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False

Sub LlenarC()
  VBCP_Update 1, "LlenarC", 1
    Combo1.Clear
  VBCP_Update 1, "LlenarC", 2
    For J = 0 To 3
  VBCP_Update 1, "LlenarC", 3
        If Option1(J) And VBCP_UpdateIf(1, "LlenarC", 4) Then
            Select Case (J)
                Case 0
  VBCP_Update 1, "LlenarC", 5
                    For I = 1 To Ndxi
  VBCP_Update 1, "LlenarC", 6
                        Combo1.AddItem (I)
  VBCP_Update 1, "LlenarC", 7
                    Next I
                Case 1
  VBCP_Update 1, "LlenarC", 8
                    For I = 1 To Ndyi
  VBCP_Update 1, "LlenarC", 9
                        Combo1.AddItem (I)
  VBCP_Update 1, "LlenarC", 10
                    Next I
                Case 2
  VBCP_Update 1, "LlenarC", 11
                    For I = 1 To Ndxs
  VBCP_Update 1, "LlenarC", 12
                        Combo1.AddItem (I)
  VBCP_Update 1, "LlenarC", 13
                    Next I
                Case 3
  VBCP_Update 1, "LlenarC", 14
                    For I = 1 To Ndyd
  VBCP_Update 1, "LlenarC", 15
                        Combo1.AddItem (I)
  VBCP_Update 1, "LlenarC", 16
                    Next I
            End Select
        End If
    Next J
    Combo1.ListIndex = 0
  VBCP_Update 1, "LlenarC", 17
    
End Sub

 Sub Check1_Click()
  VBCP_Update 1, "Check1_Click", 1
    Frame1.Enabled = (Check1 = vbChecked)
  VBCP_Update 1, "Check1_Click", 2
    Frame2.Enabled = (Check1 = vbChecked)
  VBCP_Update 1, "Check1_Click", 3
    Frame3.Enabled = (Check1 = vbChecked)
  VBCP_Update 1, "Check1_Click", 4
    Command1.Enabled = (Check1 = vbChecked)
  VBCP_Update 1, "Check1_Click", 5
End Sub


 Sub Combo1_Change()
  VBCP_Update 1, "Combo1_Change", 1
Combo1.Locked = False
  VBCP_Update 1, "Combo1_Change", 2

    
End Sub

 Sub Combo1_Click()
  VBCP_Update 1, "Combo1_Click", 1
 DiMalla
  VBCP_Update 1, "Combo1_Click", 2
 caca = True
  VBCP_Update 1, "Combo1_Click", 3
 Select Case caca
    Case Option1(0)
  VBCP_Update 1, "Combo1_Click", 4
        Text1(0) = Xi(1, Combo1.ListIndex + 1)
  VBCP_Update 1, "Combo1_Click", 5
        Text1(1) = Xi(2, Combo1.ListIndex + 1)
  VBCP_Update 1, "Combo1_Click", 6
        Picture1.Line (XU(Xi(1, Combo1.ListIndex + 1) + 1), YV(2))-(XU(Xi(2, Combo1.ListIndex + 1) + 2), YV(2)), vbBlue
  VBCP_Update 1, "Combo1_Click", 7
        
    Case Option1(1)
  VBCP_Update 1, "Combo1_Click", 8
        Text1(0) = Yi(1, Combo1.ListIndex + 1)
  VBCP_Update 1, "Combo1_Click", 9
        Text1(1) = Yi(2, Combo1.ListIndex + 1)
  VBCP_Update 1, "Combo1_Click", 10
        
        Picture1.Line (XU(2), YV(Yi(1, Combo1.ListIndex + 1) + 1))-(XU(2), YV(Yi(2, Combo1.ListIndex + 1) + 2)), vbBlue
  VBCP_Update 1, "Combo1_Click", 11
    Case Option1(2)
  VBCP_Update 1, "Combo1_Click", 12
        Text1(0) = Xs(1, Combo1.ListIndex + 1)
  VBCP_Update 1, "Combo1_Click", 13
        Text1(1) = Xs(2, Combo1.ListIndex + 1)
  VBCP_Update 1, "Combo1_Click", 14
        Picture1.Line (XU(Xs(1, Combo1.ListIndex + 1) + 1), YV(M1))-(XU(Xs(2, Combo1.ListIndex + 1) + 2), YV(M1)), vbBlue
  VBCP_Update 1, "Combo1_Click", 15
    Case Option1(3)
  VBCP_Update 1, "Combo1_Click", 16
        Text1(0) = Yd(1, Combo1.ListIndex + 1)
  VBCP_Update 1, "Combo1_Click", 17
        Text1(1) = Yd(2, Combo1.ListIndex + 1)
  VBCP_Update 1, "Combo1_Click", 18
        Picture1.Line (XU(L1), YV(Yd(1, Combo1.ListIndex + 1) + 1))-(XU(L1), YV(Yd(2, Combo1.ListIndex + 1) + 2)), vbBlue
  VBCP_Update 1, "Combo1_Click", 19
End Select
Select Case True
    Case Option1(0)
  VBCP_Update 1, "Combo1_Click", 20
        Option2(Vxi(1, Combo1.ListIndex + 1)) = True
  VBCP_Update 1, "Combo1_Click", 21
        For I = 0 To 1
  VBCP_Update 1, "Combo1_Click", 22
            Text2(I) = Vxi(I + 2, Combo1.ListIndex + 1)
  VBCP_Update 1, "Combo1_Click", 23
        Next I
    Case Option1(1)
  VBCP_Update 1, "Combo1_Click", 24
        Option2(Vyi(1, Combo1.ListIndex + 1)) = True
  VBCP_Update 1, "Combo1_Click", 25
        For I = 0 To 1
  VBCP_Update 1, "Combo1_Click", 26
            Text2(I) = Vyi(I + 2, Combo1.ListIndex + 1)
  VBCP_Update 1, "Combo1_Click", 27
        Next I
    Case Option1(2)
  VBCP_Update 1, "Combo1_Click", 28
        Option2(Vxs(1, Combo1.ListIndex + 1)) = True
  VBCP_Update 1, "Combo1_Click", 29
        For I = 0 To 1
  VBCP_Update 1, "Combo1_Click", 30
            Text2(I) = Vxs(I + 2, Combo1.ListIndex + 1)
  VBCP_Update 1, "Combo1_Click", 31
        Next I
    Case Option1(3)
  VBCP_Update 1, "Combo1_Click", 32
        Option2(Vyd(1, Combo1.ListIndex + 1)) = True
  VBCP_Update 1, "Combo1_Click", 33
        For I = 0 To 1
  VBCP_Update 1, "Combo1_Click", 34
            Text2(I) = Vyd(I + 2, Combo1.ListIndex + 1)
  VBCP_Update 1, "Combo1_Click", 35
        Next I
End Select

End Sub

 Sub Command1_Click()
  VBCP_Update 1, "Command1_Click", 1
    FTfV.Show (1)
  VBCP_Update 1, "Command1_Click", 2
    For I = 0 To 3
  VBCP_Update 1, "Command1_Click", 3
        Option1(I) = False
  VBCP_Update 1, "Command1_Click", 4
    Next I
    For I = 0 To 2
  VBCP_Update 1, "Command1_Click", 5
        Option2(I) = False
  VBCP_Update 1, "Command1_Click", 6
    Next I
    Frame4.Visible = False
  VBCP_Update 1, "Command1_Click", 7
End Sub

Sub Command2_Click()
  VBCP_Update 1, "Command2_Click", 1
  Me.Hide
  VBCP_Update 1, "Command2_Click", 2
End Sub


 Sub Form_Activate()
  VBCP_Update 1, "Form_Activate", 1
    CEscala
  VBCP_Update 1, "Form_Activate", 2
    Picture1.AutoRedraw = True
  VBCP_Update 1, "Form_Activate", 3
    DiMalla
  VBCP_Update 1, "Form_Activate", 4
    For I = 0 To 3
  VBCP_Update 1, "Form_Activate", 5
        Option1(I).Enabled = True
  VBCP_Update 1, "Form_Activate", 6
    Next I
    Select Case Simetria
        Case 0
  VBCP_Update 1, "Form_Activate", 7
            For I = 0 To 3
  VBCP_Update 1, "Form_Activate", 8
                Option1(I).Enabled = True
  VBCP_Update 1, "Form_Activate", 9
            Next I
        Case Else
  VBCP_Update 1, "Form_Activate", 10
            Option1(Simetria - 1).Enabled = False
  VBCP_Update 1, "Form_Activate", 11
            Option1(Simetria - 1) = False
  VBCP_Update 1, "Form_Activate", 12
    End Select
    Combo1 = ""
  VBCP_Update 1, "Form_Activate", 13
    Text1(0) = ""
  VBCP_Update 1, "Form_Activate", 14
    Text1(1) = ""
  VBCP_Update 1, "Form_Activate", 15
    Text2(0) = ""
  VBCP_Update 1, "Form_Activate", 16
    Text2(1) = ""
  VBCP_Update 1, "Form_Activate", 17
    For I = 0 To 3
  VBCP_Update 1, "Form_Activate", 18
        Option1(I) = False
  VBCP_Update 1, "Form_Activate", 19
    Next I
    For I = 0 To 2
  VBCP_Update 1, "Form_Activate", 20
        Option2(I) = False
  VBCP_Update 1, "Form_Activate", 21
    Next I

End Sub

Sub CEscala()
  VBCP_Update 1, "CEscala", 1
    Mediaescala = (EscalaXY * 1.1) / 2
  VBCP_Update 1, "CEscala", 2
    Picture1.ScaleHeight = -2 * Mediaescala
  VBCP_Update 1, "CEscala", 3
    Picture1.ScaleWidth = 2 * Mediaescala
  VBCP_Update 1, "CEscala", 4
    Picture1.ScaleLeft = -Mediaescala + Xl / 2
  VBCP_Update 1, "CEscala", 5
    Picture1.ScaleTop = Yl / 2 + Mediaescala
  VBCP_Update 1, "CEscala", 6
End Sub

Sub DiMalla()
  VBCP_Update 1, "DiMalla", 1
    Picture1.Cls
  VBCP_Update 1, "DiMalla", 2

     For I = 2 To L2
  VBCP_Update 1, "DiMalla", 3
         For J = 2 To M2
  VBCP_Update 1, "DiMalla", 4
             Picture1.Line (XU(I), YV(J))-(XU(I), YV(J + 1)), QBColor(8)
  VBCP_Update 1, "DiMalla", 5
             Picture1.Line (XU(I), YV(J))-(XU(I + 1), YV(J)), QBColor(8)
  VBCP_Update 1, "DiMalla", 6
         Next J
     Next I
    Picture1.Line (XU(L1), YV(M1))-(XU(L1), YV(2)), QBColor(8)
  VBCP_Update 1, "DiMalla", 7
    Picture1.Line (XU(2), YV(M1))-(XU(L1), YV(M1)), QBColor(8)
  VBCP_Update 1, "DiMalla", 8
    
    If Ns > 0 And VBCP_UpdateIf(1, "DiMalla", 9) Then
        For K = 1 To Ns
  VBCP_Update 1, "DiMalla", 10
            Picture1.Line (S(0, K), S(1, K))-(S(2, K), S(3, K)), vbMagenta, B
  VBCP_Update 1, "DiMalla", 11
        Next K
    End If
    Select Case Simetria
        Case 1
  VBCP_Update 1, "DiMalla", 12
            Picture1.Line (XU(2), YV(2))-(XU(L1), YV(2)), vbCyan
  VBCP_Update 1, "DiMalla", 13
        Case 2
  VBCP_Update 1, "DiMalla", 14
            Picture1.Line (XU(2), YV(2))-(XU(2), YV(M1)), vbCyan
  VBCP_Update 1, "DiMalla", 15
        Case 3
  VBCP_Update 1, "DiMalla", 16
            Picture1.Line (XU(L1), YV(M1))-(XU(2), YV(M1)), vbCyan
  VBCP_Update 1, "DiMalla", 17
        Case 4
  VBCP_Update 1, "DiMalla", 18
            Picture1.Line (XU(L1), YV(M1))-(XU(L1), YV(2)), vbCyan
  VBCP_Update 1, "DiMalla", 19
    End Select
End Sub

 Sub Form_Load()
  VBCP_Update 1, "Form_Load", 1
Combo1.Clear
  VBCP_Update 1, "Form_Load", 2
For I = 1 To Ndxi
  VBCP_Update 1, "Form_Load", 3
    Vxi(1, I) = 2
  VBCP_Update 1, "Form_Load", 4
    Vxi(2, I) = 0
  VBCP_Update 1, "Form_Load", 5
    Vxi(3, I) = 0
  VBCP_Update 1, "Form_Load", 6
    
Next I
For I = 1 To Ndyi
  VBCP_Update 1, "Form_Load", 7
    Vyi(1, I) = 2
  VBCP_Update 1, "Form_Load", 8
    Vyi(2, I) = 0
  VBCP_Update 1, "Form_Load", 9
    Vyi(3, I) = 0
  VBCP_Update 1, "Form_Load", 10
Next I
For I = 1 To Ndxs
  VBCP_Update 1, "Form_Load", 11
    Vxs(1, I) = 2
  VBCP_Update 1, "Form_Load", 12
    Vxs(2, I) = 0
  VBCP_Update 1, "Form_Load", 13
    Vxs(3, I) = 0
  VBCP_Update 1, "Form_Load", 14
Next I
For I = 1 To Ndyd
  VBCP_Update 1, "Form_Load", 15
    Vyd(1, I) = 2
  VBCP_Update 1, "Form_Load", 16
    Vyd(2, I) = 0
  VBCP_Update 1, "Form_Load", 17
    Vyd(3, I) = 0
  VBCP_Update 1, "Form_Load", 18
Next I

End Sub

Sub Frame1_DragDrop(Source As Control, X As Single, Y As Single)
  VBCP_Update 1, "Frame1_DragDrop", 1

End Sub

Sub Frame2_DragDrop(Source As Control, X As Single, Y As Single)
  VBCP_Update 1, "Frame2_DragDrop", 1

End Sub


 Sub Frame3_DragDrop(Source As Control, X As Single, Y As Single)
  VBCP_Update 1, "Frame3_DragDrop", 1

End Sub


 Sub Frame4_DragDrop(Source As Control, X As Single, Y As Single)
  VBCP_Update 1, "Frame4_DragDrop", 1

End Sub


 Sub Label1_Click()
  VBCP_Update 1, "Label1_Click", 1

End Sub


Sub Label2_Click(Index As Integer)
  VBCP_Update 1, "Label2_Click", 1

End Sub


Sub Label3_Click(Index As Integer)
  VBCP_Update 1, "Label3_Click", 1

End Sub


Sub Option1_Click(Index As Integer)
  VBCP_Update 1, "Option1_Click", 1
Call LlenarC
  VBCP_Update 1, "Option1_Click", 2

'For I = 0 To 4
'    Option2(I) = False
'Next I
Select Case True
    Case Option1(0)
  VBCP_Update 1, "Option1_Click", 3
        Option2(Vxi(1, Combo1.ListIndex + 1)) = True
  VBCP_Update 1, "Option1_Click", 4
        Option2(1).Enabled = True
  VBCP_Update 1, "Option1_Click", 5
        Option2(0).Enabled = True
  VBCP_Update 1, "Option1_Click", 6
    Case Option1(1)
  VBCP_Update 1, "Option1_Click", 7
        Option2(Vyi(1, Combo1.ListIndex + 1)) = True
  VBCP_Update 1, "Option1_Click", 8
        Option2(1).Enabled = False
  VBCP_Update 1, "Option1_Click", 9
        Option2(0).Enabled = False
  VBCP_Update 1, "Option1_Click", 10
    Case Option1(2)
  VBCP_Update 1, "Option1_Click", 11
        Option2(Vxs(1, Combo1.ListIndex + 1)) = True
  VBCP_Update 1, "Option1_Click", 12
        Option2(1).Enabled = True
  VBCP_Update 1, "Option1_Click", 13
        Option2(0).Enabled = True
  VBCP_Update 1, "Option1_Click", 14
    Case Option1(3)
  VBCP_Update 1, "Option1_Click", 15
        Option2(Vyd(1, Combo1.ListIndex + 1)) = True
  VBCP_Update 1, "Option1_Click", 16
        Option2(1).Enabled = False
  VBCP_Update 1, "Option1_Click", 17
        Option2(0).Enabled = False
  VBCP_Update 1, "Option1_Click", 18
End Select

End Sub


Sub Option2_Click(Index As Integer)
  VBCP_Update 1, "Option2_Click", 1
    Select Case Index
        Case 0
  VBCP_Update 1, "Option2_Click", 2
            Frame4.Visible = False
  VBCP_Update 1, "Option2_Click", 3
            For I = 0 To 1
  VBCP_Update 1, "Option2_Click", 4
                Text2(I).Visible = False
  VBCP_Update 1, "Option2_Click", 5
            Next I
            
            Select Case True
                    Case Option1(0)
  VBCP_Update 1, "Option2_Click", 6
                        Vxi(1, Combo1.ListIndex + 1) = Index
  VBCP_Update 1, "Option2_Click", 7
                    Case Option1(1)
  VBCP_Update 1, "Option2_Click", 8
                        Vyi(1, Combo1.ListIndex + 1) = Index
  VBCP_Update 1, "Option2_Click", 9
                    Case Option1(2)
  VBCP_Update 1, "Option2_Click", 10
                        Vxs(1, Combo1.ListIndex + 1) = Index
  VBCP_Update 1, "Option2_Click", 11
                    Case Option1(3)
  VBCP_Update 1, "Option2_Click", 12
                        Vyd(1, Combo1.ListIndex + 1) = Index
  VBCP_Update 1, "Option2_Click", 13
                End Select
            

        Case 1
  VBCP_Update 1, "Option2_Click", 14
            Frame4.Visible = True
  VBCP_Update 1, "Option2_Click", 15
            Text2(1).Visible = True
  VBCP_Update 1, "Option2_Click", 16
            Text2(0).Visible = True
  VBCP_Update 1, "Option2_Click", 17
            Label3(0) = "v ext."
  VBCP_Update 1, "Option2_Click", 18
            Label3(1) = "v cen."
  VBCP_Update 1, "Option2_Click", 19
            Label3(1).Visible = True
  VBCP_Update 1, "Option2_Click", 20
                Select Case True
                    Case Option1(0)
  VBCP_Update 1, "Option2_Click", 21
                        For I = 0 To 1
  VBCP_Update 1, "Option2_Click", 22
                            Text2(I) = Vxi(I + 2, Combo1.ListIndex + 1)
  VBCP_Update 1, "Option2_Click", 23
                        Next I
                        Vxi(1, Combo1.ListIndex + 1) = Index
  VBCP_Update 1, "Option2_Click", 24
                    Case Option1(1)
  VBCP_Update 1, "Option2_Click", 25
                        For I = 0 To 1
  VBCP_Update 1, "Option2_Click", 26
                            Text2(I) = Vyi(I + 2, Combo1.ListIndex + 1)
  VBCP_Update 1, "Option2_Click", 27
                        Next I
                        Vyi(1, Combo1.ListIndex + 1) = Index
  VBCP_Update 1, "Option2_Click", 28
                    Case Option1(2)
  VBCP_Update 1, "Option2_Click", 29
                        For I = 0 To 1
  VBCP_Update 1, "Option2_Click", 30
                            Text2(I) = Vxs(I + 2, Combo1.ListIndex + 1)
  VBCP_Update 1, "Option2_Click", 31
                        Next I
                        Vxs(1, Combo1.ListIndex + 1) = Index
  VBCP_Update 1, "Option2_Click", 32
                    Case Option1(3)
  VBCP_Update 1, "Option2_Click", 33
                        For I = 0 To 1
  VBCP_Update 1, "Option2_Click", 34
                            Text2(I) = Vyd(I + 2, Combo1.ListIndex + 1)
  VBCP_Update 1, "Option2_Click", 35
                        Next I
                        Vyd(1, Combo1.ListIndex + 1) = Index
  VBCP_Update 1, "Option2_Click", 36
                    End Select
            
            
        Case 2
  VBCP_Update 1, "Option2_Click", 37
            Frame4.Visible = True
  VBCP_Update 1, "Option2_Click", 38
            Text2(1).Visible = False
  VBCP_Update 1, "Option2_Click", 39
            Text2(0).Visible = True
  VBCP_Update 1, "Option2_Click", 40
            Label3(0) = "v"
  VBCP_Update 1, "Option2_Click", 41
            Label3(1).Visible = False
  VBCP_Update 1, "Option2_Click", 42
            
                Select Case True
                    Case Option1(0)
  VBCP_Update 1, "Option2_Click", 43
                        For I = 0 To 1
  VBCP_Update 1, "Option2_Click", 44
                            Text2(I) = Vxi(I + 2, Combo1.ListIndex + 1)
  VBCP_Update 1, "Option2_Click", 45
                        Next I
                        Vxi(1, Combo1.ListIndex + 1) = Index
  VBCP_Update 1, "Option2_Click", 46
                    Case Option1(1)
  VBCP_Update 1, "Option2_Click", 47
                        For I = 0 To 1
  VBCP_Update 1, "Option2_Click", 48
                            Text2(I) = Vyi(I + 2, Combo1.ListIndex + 1)
  VBCP_Update 1, "Option2_Click", 49
                        Next I
                        Vyi(1, Combo1.ListIndex + 1) = Index
  VBCP_Update 1, "Option2_Click", 50
                    Case Option1(2)
  VBCP_Update 1, "Option2_Click", 51
                        For I = 0 To 1
  VBCP_Update 1, "Option2_Click", 52
                            Text2(I) = Vxs(I + 2, Combo1.ListIndex + 1)
  VBCP_Update 1, "Option2_Click", 53
                        Next I
                        Vxs(1, Combo1.ListIndex + 1) = Index
  VBCP_Update 1, "Option2_Click", 54
                    Case Option1(3)
  VBCP_Update 1, "Option2_Click", 55
                        For I = 0 To 1
  VBCP_Update 1, "Option2_Click", 56
                            Text2(I) = Vyd(I + 2, Combo1.ListIndex + 1)
  VBCP_Update 1, "Option2_Click", 57
                        Next I
                        Vyd(1, Combo1.ListIndex + 1) = Index
  VBCP_Update 1, "Option2_Click", 58
                End Select
    End Select
End Sub


Sub Picture1_Click()
  VBCP_Update 1, "Picture1_Click", 1

End Sub


Sub Text1_Change(Index As Integer)
  VBCP_Update 1, "Text1_Change", 1

End Sub


Sub Text2_Change(Index As Integer)
  VBCP_Update 1, "Text2_Change", 1
Select Case True
    Case Option1(0)
  VBCP_Update 1, "Text2_Change", 2
        If Index = 0 And VBCP_UpdateIf(1, "Text2_Change", 3) Then
             Vxi(Index + 2, Combo1.ListIndex + 1) = Val(Text2(Index))
  VBCP_Update 1, "Text2_Change", 4
        Else
            Vxi(Index + 2, Combo1.ListIndex + 1) = Val(Text2(Index))
  VBCP_Update 1, "Text2_Change", 5
        End If
        Case Option1(1)
  VBCP_Update 1, "Text2_Change", 6
        
        If Index = 0 And VBCP_UpdateIf(1, "Text2_Change", 7) Then
             Vyi(Index + 2, Combo1.ListIndex + 1) = Val(Text2(Index))
  VBCP_Update 1, "Text2_Change", 8
        Else
            Vyi(Index + 2, Combo1.ListIndex + 1) = Val(Text2(Index))
  VBCP_Update 1, "Text2_Change", 9
        End If
        
        Case Option1(2)
  VBCP_Update 1, "Text2_Change", 10
        
        If Index = 0 And VBCP_UpdateIf(1, "Text2_Change", 11) Then
             Vxs(Index + 2, Combo1.ListIndex + 1) = Val(Text2(Index))
  VBCP_Update 1, "Text2_Change", 12
        Else
            Vxs(Index + 2, Combo1.ListIndex + 1) = Val(Text2(Index))
  VBCP_Update 1, "Text2_Change", 13
        End If
        
        Case Option1(3)
  VBCP_Update 1, "Text2_Change", 14
        
        If Index = 0 And VBCP_UpdateIf(1, "Text2_Change", 15) Then
             Vyd(Index + 2, Combo1.ListIndex + 1) = Val(Text2(Index))
  VBCP_Update 1, "Text2_Change", 16
        Else
            Vyd(Index + 2, Combo1.ListIndex + 1) = Val(Text2(Index))
  VBCP_Update 1, "Text2_Change", 17
        End If
        
End Select
End Sub



