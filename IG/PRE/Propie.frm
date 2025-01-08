VERSION 5.00
Begin VB.Form Fpro 
   BorderStyle     =   4  'Fixed ToolWindow
   Caption         =   "Propiedades"
   ClientHeight    =   3195
   ClientLeft      =   3330
   ClientTop       =   2295
   ClientWidth     =   3375
   ControlBox      =   0   'False
   LinkTopic       =   "Form4"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   3195
   ScaleWidth      =   3375
   ShowInTaskbar   =   0   'False
   StartUpPosition =   2  'CenterScreen
   Begin VB.TextBox Text1 
      Height          =   315
      Index           =   4
      Left            =   1680
      TabIndex        =   9
      Text            =   "0.1"
      Top             =   2040
      Width           =   1215
   End
   Begin VB.TextBox Text1 
      Height          =   315
      Index           =   3
      Left            =   1680
      TabIndex        =   7
      Text            =   "0.1"
      Top             =   1560
      Width           =   1215
   End
   Begin VB.TextBox Text1 
      Height          =   315
      Index           =   2
      Left            =   1680
      TabIndex        =   5
      Text            =   "0.1"
      Top             =   1080
      Width           =   1215
   End
   Begin VB.CommandButton Command3 
      Caption         =   "Aceptar"
      Height          =   495
      Left            =   1080
      TabIndex        =   4
      Top             =   2580
      Width           =   1215
   End
   Begin VB.TextBox Text1 
      Height          =   315
      Index           =   1
      Left            =   1680
      TabIndex        =   2
      Text            =   "0.1"
      Top             =   600
      Width           =   1215
   End
   Begin VB.TextBox Text1 
      Height          =   315
      Index           =   0
      Left            =   1680
      TabIndex        =   0
      Text            =   "0.1"
      Top             =   180
      Width           =   1215
   End
   Begin VB.Label Label5 
      Caption         =   "b"
      BeginProperty Font 
         Name            =   "Symbol"
         Size            =   9.75
         Charset         =   2
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   195
      Left            =   480
      TabIndex        =   10
      Top             =   2100
      Width           =   1335
   End
   Begin VB.Label Label2 
      Caption         =   "Cp"
      Height          =   195
      Left            =   480
      TabIndex        =   8
      Top             =   1620
      Width           =   1335
   End
   Begin VB.Label Label1 
      Caption         =   "Viscosidad"
      Height          =   195
      Left            =   480
      TabIndex        =   6
      Top             =   1140
      Width           =   1335
   End
   Begin VB.Label Label4 
      Caption         =   "Conductividad"
      Height          =   195
      Left            =   480
      TabIndex        =   3
      Top             =   660
      Width           =   1335
   End
   Begin VB.Label Label3 
      Caption         =   "Densidad"
      Height          =   195
      Left            =   480
      TabIndex        =   1
      Top             =   240
      Width           =   1335
   End
End
Attribute VB_Name = "Fpro"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Sub Command1_Click()
  VBCP_Update 13, "Command1_Click", 1

End Sub

Private Sub Command3_Click()
  VBCP_Update 13, "Command3_Click", 1
    
    For kk = 0 To 4
  VBCP_Update 13, "Command3_Click", 2
        Pro(kk) = Val(Text1(kk))
  VBCP_Update 13, "Command3_Click", 3
    Next kk
    Fpro.Hide
  VBCP_Update 13, "Command3_Click", 4
End Sub


Private Sub Form_Load()
  VBCP_Update 13, "Form_Load", 1
For kk = 0 To 4
  VBCP_Update 13, "Form_Load", 2
        Pro(kk) = Val(Text1(kk))
  VBCP_Update 13, "Form_Load", 3
    Next kk
End Sub


Private Sub Text1_Change(Index As Integer)
  VBCP_Update 13, "Text1_Change", 1

End Sub



