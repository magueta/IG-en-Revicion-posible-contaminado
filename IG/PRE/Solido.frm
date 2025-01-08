VERSION 5.00
Begin VB.Form Fsolido 
   BorderStyle     =   4  'Fixed ToolWindow
   Caption         =   "Sólido"
   ClientHeight    =   4095
   ClientLeft      =   885
   ClientTop       =   1395
   ClientWidth     =   3975
   ClipControls    =   0   'False
   ControlBox      =   0   'False
   LinkTopic       =   "Form3"
   LockControls    =   -1  'True
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   4095
   ScaleWidth      =   3975
   ShowInTaskbar   =   0   'False
   StartUpPosition =   2  'CenterScreen
   Begin VB.Frame Frame1 
      Caption         =   "Propiedades"
      Height          =   1695
      Left            =   240
      TabIndex        =   13
      Top             =   1560
      Width           =   3495
      Begin VB.TextBox Text1 
         Height          =   315
         Index           =   6
         Left            =   1800
         TabIndex        =   7
         Text            =   "1"
         Top             =   1200
         Width           =   1215
      End
      Begin VB.TextBox Text1 
         Height          =   315
         Index           =   4
         Left            =   1800
         TabIndex        =   5
         Text            =   "1"
         Top             =   360
         Width           =   1215
      End
      Begin VB.TextBox Text1 
         Height          =   315
         Index           =   5
         Left            =   1800
         TabIndex        =   6
         Text            =   "1"
         Top             =   780
         Width           =   1215
      End
      Begin VB.Label Label8 
         Caption         =   "Cp"
         Height          =   195
         Left            =   360
         TabIndex        =   17
         Top             =   1260
         Width           =   1335
      End
      Begin VB.Label Label6 
         Caption         =   "Densidad"
         Height          =   195
         Left            =   360
         TabIndex        =   15
         Top             =   840
         Width           =   1335
      End
      Begin VB.Label Label5 
         Caption         =   "Conductividad"
         Height          =   195
         Left            =   360
         TabIndex        =   14
         Top             =   420
         Width           =   1335
      End
   End
   Begin VB.CommandButton Command3 
      Cancel          =   -1  'True
      Caption         =   "Cancelar"
      Height          =   495
      Left            =   2400
      TabIndex        =   12
      Top             =   3480
      Width           =   1215
   End
   Begin VB.TextBox Text1 
      Height          =   315
      Index           =   3
      Left            =   2460
      TabIndex        =   4
      Top             =   540
      Width           =   975
   End
   Begin VB.TextBox Text1 
      Height          =   315
      Index           =   2
      Left            =   2460
      TabIndex        =   3
      Top             =   120
      Width           =   975
   End
   Begin VB.TextBox Text1 
      Height          =   315
      Index           =   1
      Left            =   900
      TabIndex        =   2
      Top             =   540
      Width           =   975
   End
   Begin VB.CommandButton Command2 
      Caption         =   "Aceptar"
      Default         =   -1  'True
      Height          =   495
      Left            =   540
      TabIndex        =   11
      Top             =   3480
      Width           =   1215
   End
   Begin VB.TextBox Text1 
      Height          =   315
      Index           =   0
      Left            =   900
      TabIndex        =   1
      Top             =   120
      Width           =   975
   End
   Begin VB.Label Label7 
      BorderStyle     =   1  'Fixed Single
      Caption         =   "Label7"
      Height          =   495
      Left            =   240
      TabIndex        =   16
      Top             =   960
      Width           =   3495
   End
   Begin VB.Label Label4 
      Caption         =   "Y2"
      Height          =   195
      Left            =   2040
      TabIndex        =   10
      Top             =   600
      Width           =   1215
   End
   Begin VB.Label Label3 
      Caption         =   "X2"
      Height          =   195
      Left            =   2040
      TabIndex        =   9
      Top             =   180
      Width           =   1215
   End
   Begin VB.Label Label2 
      Caption         =   "Y1"
      Height          =   195
      Left            =   480
      TabIndex        =   8
      Top             =   600
      Width           =   1215
   End
   Begin VB.Label Label1 
      Caption         =   "X1"
      Height          =   195
      Left            =   480
      TabIndex        =   0
      Top             =   180
      Width           =   1215
   End
End
Attribute VB_Name = "Fsolido"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False


Private Sub Command2_Click()
  VBCP_Update 14, "Command2_Click", 1
    For K = 0 To 6
  VBCP_Update 14, "Command2_Click", 2
        S(K, Ns) = Val(Text1(K))
  VBCP_Update 14, "Command2_Click", 3
    Next K
    Fsolido.Hide
  VBCP_Update 14, "Command2_Click", 4
End Sub

Private Sub Command3_Click()
  VBCP_Update 14, "Command3_Click", 1
    Ns = Ns - 1
  VBCP_Update 14, "Command3_Click", 2
    ReDim Preserve S(6, Ns)
  VBCP_Update 14, "Command3_Click", 3
    Fsolido.Hide
  VBCP_Update 14, "Command3_Click", 4

End Sub


Private Sub Form_Activate()
  VBCP_Update 14, "Form_Activate", 1
    Label7 = "Xmax= " & Xl & Chr$(13) & " Ymax= " & Yl
  VBCP_Update 14, "Form_Activate", 2
    Ns = Ns + 1
  VBCP_Update 14, "Form_Activate", 3
    ReDim Preserve S(6, Ns)
  VBCP_Update 14, "Form_Activate", 4

End Sub




Private Sub Text1_Change(Index As Integer)
  VBCP_Update 14, "Text1_Change", 1

End Sub



