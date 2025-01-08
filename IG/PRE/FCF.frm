VERSION 5.00
Begin VB.Form FCF 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Condiciones de Fronteras"
   ClientHeight    =   4785
   ClientLeft      =   945
   ClientTop       =   495
   ClientWidth     =   4620
   ControlBox      =   0   'False
   LinkTopic       =   "Form1"
   LockControls    =   -1  'True
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   4785
   ScaleWidth      =   4620
   Begin VB.CommandButton Command5 
      Caption         =   "Seguir"
      Height          =   435
      Left            =   1110
      TabIndex        =   23
      Top             =   4260
      Width           =   2355
   End
   Begin VB.CommandButton Command4 
      Caption         =   "Ecuación de Momento en Y"
      Enabled         =   0   'False
      Height          =   435
      Left            =   1110
      TabIndex        =   22
      Top             =   3660
      Width           =   2355
   End
   Begin VB.CommandButton Command3 
      Caption         =   "Ecuación de Momento en X"
      Enabled         =   0   'False
      Height          =   435
      Left            =   1110
      TabIndex        =   21
      Top             =   3120
      Width           =   2355
   End
   Begin VB.CommandButton Command2 
      Caption         =   "Ecuación de Energía"
      Enabled         =   0   'False
      Height          =   435
      Left            =   1110
      TabIndex        =   20
      Top             =   2580
      Width           =   2355
   End
   Begin VB.Frame Frame1 
      Caption         =   "Regiones de Fronteras"
      Height          =   2415
      Left            =   60
      TabIndex        =   0
      Top             =   60
      Width           =   4455
      Begin VB.CommandButton Command1 
         Caption         =   "Aceptar"
         Height          =   315
         Left            =   1740
         TabIndex        =   19
         Top             =   1980
         Width           =   975
      End
      Begin VB.OptionButton Option1 
         Enabled         =   0   'False
         Height          =   195
         Index           =   3
         Left            =   3600
         TabIndex        =   18
         Top             =   1620
         Width           =   195
      End
      Begin VB.CheckBox Check2 
         Height          =   195
         Index           =   3
         Left            =   2220
         TabIndex        =   17
         Top             =   1620
         Width           =   195
      End
      Begin VB.TextBox Text1 
         Height          =   285
         Index           =   3
         Left            =   1080
         TabIndex        =   16
         Text            =   "1"
         Top             =   1560
         Width           =   615
      End
      Begin VB.OptionButton Option1 
         Enabled         =   0   'False
         Height          =   195
         Index           =   2
         Left            =   3600
         TabIndex        =   14
         Top             =   1260
         Width           =   195
      End
      Begin VB.CheckBox Check2 
         Height          =   195
         Index           =   2
         Left            =   2220
         TabIndex        =   13
         Top             =   1260
         Width           =   195
      End
      Begin VB.TextBox Text1 
         Height          =   285
         Index           =   2
         Left            =   1080
         TabIndex        =   12
         Text            =   "1"
         Top             =   1200
         Width           =   615
      End
      Begin VB.OptionButton Option1 
         Enabled         =   0   'False
         Height          =   195
         Index           =   1
         Left            =   3600
         TabIndex        =   10
         Top             =   900
         Width           =   195
      End
      Begin VB.CheckBox Check2 
         Height          =   195
         Index           =   1
         Left            =   2220
         TabIndex        =   9
         Top             =   900
         Width           =   195
      End
      Begin VB.TextBox Text1 
         Height          =   285
         Index           =   1
         Left            =   1080
         TabIndex        =   8
         Text            =   "1"
         Top             =   840
         Width           =   615
      End
      Begin VB.OptionButton Option1 
         Enabled         =   0   'False
         Height          =   195
         Index           =   0
         Left            =   3600
         TabIndex        =   5
         Top             =   540
         Width           =   195
      End
      Begin VB.CheckBox Check2 
         Height          =   195
         Index           =   0
         Left            =   2220
         TabIndex        =   4
         Top             =   540
         Width           =   195
      End
      Begin VB.CheckBox Check1 
         Caption         =   "Eje de Simetría"
         Height          =   195
         Left            =   2970
         TabIndex        =   3
         Top             =   240
         Width           =   1455
      End
      Begin VB.TextBox Text1 
         Height          =   285
         Index           =   0
         Left            =   1080
         TabIndex        =   2
         Text            =   "1"
         Top             =   480
         Width           =   615
      End
      Begin VB.Label Label1 
         Caption         =   "Derecha"
         Height          =   255
         Index           =   3
         Left            =   180
         TabIndex        =   15
         Top             =   1620
         Width           =   855
      End
      Begin VB.Label Label1 
         Caption         =   "Superior"
         Height          =   255
         Index           =   2
         Left            =   180
         TabIndex        =   11
         Top             =   1260
         Width           =   855
      End
      Begin VB.Label Label1 
         Caption         =   "Izquierda"
         Height          =   255
         Index           =   1
         Left            =   180
         TabIndex        =   7
         Top             =   900
         Width           =   855
      End
      Begin VB.Label Label2 
         Caption         =   "Por Bloques"
         Height          =   195
         Left            =   1800
         TabIndex        =   6
         Top             =   240
         Width           =   1035
      End
      Begin VB.Label Label1 
         Caption         =   "Inferior"
         Height          =   255
         Index           =   0
         Left            =   180
         TabIndex        =   1
         Top             =   540
         Width           =   855
      End
   End
End
Attribute VB_Name = "FCF"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False



 Sub Check1_Click()
  VBCP_Update 5, "Check1_Click", 1
    If Check1 = vbChecked And VBCP_UpdateIf(5, "Check1_Click", 2) Then
        For I = 0 To 3
  VBCP_Update 5, "Check1_Click", 3
            Option1(I).Enabled = True
  VBCP_Update 5, "Check1_Click", 4
            Option1(0) = Not Check1
  VBCP_Update 5, "Check1_Click", 5
        Next I
    Else
        For I = 0 To 3
  VBCP_Update 5, "Check1_Click", 6
            Option1(I).Enabled = False
  VBCP_Update 5, "Check1_Click", 7
            Option1(I) = False
  VBCP_Update 5, "Check1_Click", 8
            Check2(I).Enabled = True
  VBCP_Update 5, "Check1_Click", 9
            Text1(I).Enabled = Not (vbChecked = Check2(I))
  VBCP_Update 5, "Check1_Click", 10
        Next I
        Simetria = 0
  VBCP_Update 5, "Check1_Click", 11
    End If
    Command2.Enabled = False
  VBCP_Update 5, "Check1_Click", 12
    Command3.Enabled = False
  VBCP_Update 5, "Check1_Click", 13
    Command4.Enabled = False
  VBCP_Update 5, "Check1_Click", 14
   
End Sub

 Sub Check2_Click(Index As Integer)
  VBCP_Update 5, "Check2_Click", 1
    Select Case Index
        Case 0
  VBCP_Update 5, "Check2_Click", 2
            Text1(Index).Enabled = Not (vbChecked = Check2(Index))
  VBCP_Update 5, "Check2_Click", 3
            Text1(Index) = Ndx
  VBCP_Update 5, "Check2_Click", 4
        Case 1
  VBCP_Update 5, "Check2_Click", 5
            Text1(Index).Enabled = Not (vbChecked = Check2(Index))
  VBCP_Update 5, "Check2_Click", 6
            Text1(Index) = Ndy
  VBCP_Update 5, "Check2_Click", 7
        Case 2
  VBCP_Update 5, "Check2_Click", 8
            Text1(Index).Enabled = Not (vbChecked = Check2(Index))
  VBCP_Update 5, "Check2_Click", 9
            Text1(Index) = Ndx
  VBCP_Update 5, "Check2_Click", 10
        Case 3
  VBCP_Update 5, "Check2_Click", 11
            Text1(Index).Enabled = Not (vbChecked = Check2(Index))
  VBCP_Update 5, "Check2_Click", 12
            Text1(Index) = Ndy
  VBCP_Update 5, "Check2_Click", 13
    End Select
    Command2.Enabled = False
  VBCP_Update 5, "Check2_Click", 14
    Command3.Enabled = False
  VBCP_Update 5, "Check2_Click", 15
    Command4.Enabled = False
  VBCP_Update 5, "Check2_Click", 16
    
End Sub

Private Sub Command1_Click()
  VBCP_Update 5, "Command1_Click", 1
    Call ObtenerE
  VBCP_Update 5, "Command1_Click", 2
    For I = 0 To 3
  VBCP_Update 5, "Command1_Click", 3
        If Not (Check2(I).Value = vbChecked) _
        And Val(Text1(I)) > 1 Then
  VBCP_Update 5, "Command1_Click", 4
            FRegion.Show (vbModal)
            Exit For
  VBCP_Update 5, "Command1_Click", 5
        End If
    Next I
    Command2.Enabled = True
  VBCP_Update 5, "Command1_Click", 6
    Command3.Enabled = True
  VBCP_Update 5, "Command1_Click", 7
    Command4.Enabled = True
  VBCP_Update 5, "Command1_Click", 8
    
    
End Sub

 Sub Command2_Click()
  VBCP_Update 5, "Command2_Click", 1
    FE.Show (1)
  VBCP_Update 5, "Command2_Click", 2
End Sub

 Sub Command3_Click()
  VBCP_Update 5, "Command3_Click", 1
    FU.Show (1)
  VBCP_Update 5, "Command3_Click", 2
End Sub


 Sub Command4_Click()
  VBCP_Update 5, "Command4_Click", 1
    FV.Show (1)
  VBCP_Update 5, "Command4_Click", 2
End Sub

Sub Command5_Click()
  VBCP_Update 5, "Command5_Click", 1
    Me.Hide
  VBCP_Update 5, "Command5_Click", 2
End Sub


 Sub Form_Activate()
  VBCP_Update 5, "Form_Activate", 1
 
 ReDim Preserve TFE(2, Ndx * Ndy) As Single
  VBCP_Update 5, "Form_Activate", 2

 End Sub

 Sub Form_Load()
  VBCP_Update 5, "Form_Load", 1
Ndxi = 1
  VBCP_Update 5, "Form_Load", 2
Ndxs = 1
  VBCP_Update 5, "Form_Load", 3
Ndyi = 1
  VBCP_Update 5, "Form_Load", 4
Ndyd = 1
  VBCP_Update 5, "Form_Load", 5
End Sub

 Sub Label1_Click(Index As Integer)
  VBCP_Update 5, "Label1_Click", 1

End Sub


 Sub Option1_Click(Index As Integer)
  VBCP_Update 5, "Option1_Click", 1
    For J = 0 To 3
  VBCP_Update 5, "Option1_Click", 2
        Text1(J).Enabled = True
  VBCP_Update 5, "Option1_Click", 3
        Check2(J).Enabled = True
  VBCP_Update 5, "Option1_Click", 4
        
    Next J
    Check2(Index) = 0
  VBCP_Update 5, "Option1_Click", 5
    Text1(Index).Enabled = Not Option1(Index)
  VBCP_Update 5, "Option1_Click", 6
    Check2(Index).Enabled = Not Option1(Index)
  VBCP_Update 5, "Option1_Click", 7
    Text1(Index) = 1
  VBCP_Update 5, "Option1_Click", 8
    Command2.Enabled = False
  VBCP_Update 5, "Option1_Click", 9
    Command3.Enabled = False
  VBCP_Update 5, "Option1_Click", 10
    Command4.Enabled = False
  VBCP_Update 5, "Option1_Click", 11
    
    If Option1(Index) And VBCP_UpdateIf(5, "Option1_Click", 12) Then
        Simetria = Index + 1
  VBCP_Update 5, "Option1_Click", 13
    Else
        Simetria = 0
  VBCP_Update 5, "Option1_Click", 14
    End If
    
End Sub

 Sub Text1_Change(Index As Integer)
  VBCP_Update 5, "Text1_Change", 1
    Select Case Index
        Case 0
  VBCP_Update 5, "Text1_Change", 2
            Ndxi = Val(Text1(Index))
  VBCP_Update 5, "Text1_Change", 3
        Case 1
  VBCP_Update 5, "Text1_Change", 4
            Ndyi = Val(Text1(Index))
  VBCP_Update 5, "Text1_Change", 5
        Case 2
  VBCP_Update 5, "Text1_Change", 6
            Ndxs = Val(Text1(Index))
  VBCP_Update 5, "Text1_Change", 7
        Case 3
  VBCP_Update 5, "Text1_Change", 8
            Ndyd = Val(Text1(Index))
  VBCP_Update 5, "Text1_Change", 9
    End Select
    Command2.Enabled = False
  VBCP_Update 5, "Text1_Change", 10
    Command3.Enabled = False
  VBCP_Update 5, "Text1_Change", 11
    Command4.Enabled = False
  VBCP_Update 5, "Text1_Change", 12
  
End Sub



