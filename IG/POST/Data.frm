VERSION 5.00
Object = "{5E9E78A0-531B-11CF-91F6-C2863C385E30}#1.0#0"; "MSFLXGRD.OCX"
Begin VB.Form Data 
   AutoRedraw      =   -1  'True
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Data"
   ClientHeight    =   6225
   ClientLeft      =   255
   ClientTop       =   690
   ClientWidth     =   9060
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   ScaleHeight     =   6225
   ScaleWidth      =   9060
   StartUpPosition =   2  'CenterScreen
   Begin VB.CommandButton Command2 
      Caption         =   "Guardar"
      Height          =   375
      Left            =   660
      TabIndex        =   6
      Top             =   2700
      Width           =   915
   End
   Begin VB.CommandButton Command1 
      Caption         =   "Ocultar"
      Height          =   375
      Left            =   660
      TabIndex        =   5
      Top             =   3360
      Width           =   915
   End
   Begin VB.OptionButton Option1 
      Caption         =   "Presión"
      Height          =   195
      Index           =   3
      Left            =   540
      TabIndex        =   4
      Top             =   1740
      Width           =   1215
   End
   Begin VB.OptionButton Option1 
      Caption         =   "Velocidad v"
      Height          =   195
      Index           =   2
      Left            =   540
      TabIndex        =   3
      Top             =   1440
      Width           =   1215
   End
   Begin VB.OptionButton Option1 
      Caption         =   "Velocidad u"
      Height          =   195
      Index           =   1
      Left            =   540
      TabIndex        =   2
      Top             =   1140
      Width           =   1215
   End
   Begin VB.OptionButton Option1 
      Caption         =   "Temperatura"
      Height          =   195
      Index           =   0
      Left            =   540
      TabIndex        =   1
      Top             =   840
      Value           =   -1  'True
      Width           =   1215
   End
   Begin MSFlexGridLib.MSFlexGrid MSFlexGrid1 
      Height          =   5955
      Left            =   2280
      TabIndex        =   0
      Top             =   120
      Width           =   6615
      _ExtentX        =   11668
      _ExtentY        =   10504
      _Version        =   327680
      AllowBigSelection=   -1  'True
      ScrollTrack     =   -1  'True
      AllowUserResizing=   3
      FormatString    =   "Y\X            "
      OLEDropMode     =   1
   End
End
Attribute VB_Name = "Data"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False





Private Sub Command1_Click()
Me.Hide
Post.Show
End Sub

Private Sub Command2_Click()
Titulo = InputBox("Nombre del Archivo (sin extención)", "Guardar datos en txt")
If Titulo <> "" Then
    Open Titulo & ".txt" For Output As #1
    With MSFlexGrid1
        For j = 0 To .Rows - 1
             .Row = j
            .Col = 0
            .ColSel = .Cols - 1
            Print #1, .Clip
        Next j
    End With
    Close #1
End If
End Sub


Private Sub Form_Load()
MSFlexGrid1.Cols = L1 + 1
MSFlexGrid1.Rows = M1 + 1
With MSFlexGrid1
    For i = 1 To L1
        For j = M1 To 1 Step -1
            .Col = i
            .Row = M1 - j + 1
            .Text = Format(T(i, j), "0.0000e+00")
        Next j
    Next i
    .Row = 0
    For i = 1 To L1
        .Col = i
        .Text = Format(x(i), "0.000e+00")
    Next i
    .Col = 0
    For j = M1 To 1 Step -1
        .Row = M1 - j + 1
        .Text = Format(y(j), "0.000e+00")
    Next j
'    For j = 0 To .Rows - 1
'         .Row = j
'        .Col = 0
'        .ColSel = .Cols - 1
'        Debug.Print .Clip
'    Next j
End With
End Sub


Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)
End
End Sub


Private Sub Option1_Click(Index As Integer)
MSFlexGrid1.Cols = L1 + 1
MSFlexGrid1.Rows = M1 + 1
With MSFlexGrid1
    For i = 1 To L1
        For j = M1 To 1 Step -1
            .Col = i
            .Row = M1 - j + 1
            Select Case Index
                Case 0
                    .Text = Format(T(i, j), "0.0000e+00")
                Case 1
                    .Text = Format(uN(i, j), "0.0000e+00")
                Case 2
                    .Text = Format(vN(i, j), "0.0000e+00")
                Case 3
                    .Text = Format(P(i, j), "0.0000e+00")
            End Select
        Next j
    Next i
    .Row = 0
    For i = 1 To L1
        .Col = i
        .Text = Format(x(i), "0.000e+00")
    Next i
    .Col = 0
    For j = M1 To 1 Step -1
        .Row = M1 - j + 1
        .Text = Format(y(j), "0.000e+00")
    Next j
'    For j = 0 To .Rows - 1
'         .Row = j
'        .Col = 0
'        .ColSel = .Cols - 1
'        Debug.Print .Clip
'    Next j
End With
End Sub


