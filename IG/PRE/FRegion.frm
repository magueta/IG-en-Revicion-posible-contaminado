VERSION 5.00
Begin VB.Form FRegion 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Regiones"
   ClientHeight    =   2865
   ClientLeft      =   5115
   ClientTop       =   2985
   ClientWidth     =   2640
   ClipControls    =   0   'False
   ControlBox      =   0   'False
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   2865
   ScaleWidth      =   2640
   Begin VB.ComboBox Combo1 
      Height          =   315
      Left            =   1080
      TabIndex        =   10
      Text            =   "Combo1"
      Top             =   1440
      Width           =   1095
   End
   Begin VB.VScrollBar VScroll2 
      Height          =   315
      Left            =   2280
      TabIndex        =   9
      Top             =   2100
      Width           =   195
   End
   Begin VB.CommandButton Command2 
      Caption         =   "Seguir"
      Height          =   255
      Left            =   480
      TabIndex        =   8
      Top             =   2520
      Width           =   1575
   End
   Begin VB.Frame Frame1 
      Caption         =   "Frontera"
      Height          =   1215
      Left            =   360
      TabIndex        =   3
      Top             =   180
      Width           =   1755
      Begin VB.OptionButton Option1 
         Caption         =   "Inferior"
         Height          =   195
         Index           =   0
         Left            =   240
         TabIndex        =   7
         Top             =   240
         Width           =   1155
      End
      Begin VB.OptionButton Option1 
         Caption         =   "Izquierda"
         Height          =   195
         Index           =   1
         Left            =   240
         TabIndex        =   6
         Top             =   480
         Width           =   1155
      End
      Begin VB.OptionButton Option1 
         Caption         =   "Superior"
         Height          =   195
         Index           =   2
         Left            =   240
         TabIndex        =   5
         Top             =   720
         Width           =   1155
      End
      Begin VB.OptionButton Option1 
         Caption         =   "Derecha"
         Height          =   195
         Index           =   3
         Left            =   240
         TabIndex        =   4
         Top             =   960
         Width           =   1155
      End
   End
   Begin VB.TextBox Text2 
      Height          =   285
      Left            =   780
      Locked          =   -1  'True
      TabIndex        =   1
      Top             =   2100
      Width           =   1455
   End
   Begin VB.Label Label3 
      Caption         =   "Región Nº"
      Height          =   195
      Left            =   240
      TabIndex        =   11
      Top             =   1500
      Width           =   735
   End
   Begin VB.Label Label2 
      Caption         =   "Label2"
      Height          =   255
      Left            =   120
      TabIndex        =   2
      Top             =   1800
      Width           =   1515
   End
   Begin VB.Label Label1 
      Caption         =   "Label1"
      Height          =   255
      Left            =   120
      TabIndex        =   0
      Top             =   2100
      Width           =   555
   End
End
Attribute VB_Name = "FRegion"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False

Private Sub Combo1_Change()
  VBCP_Update 18, "Combo1_Change", 1
' caca = True
'     Select Case caca
'        Case Option1(0)
'            Label2 = "xmin.= " & XU(Xi(1, Combo1.ListIndex + 1) + 1)
'            VScroll2.Min = Xi(1, Combo1.ListIndex + 1) + 2
'            VScroll2.Max = L1 + (Combo1.ListIndex + 1) - Ndxi
'            VScroll2 = Xi(2, Combo1.ListIndex + 1) + 2
'            Label1 = "xmax.= "
'        Case Option1(1)
'            Label2 = "ymin.= " & YV(Yi(1, Combo1.ListIndex + 1) + 1)
'            VScroll2.Min = Yi(1, Combo1.ListIndex + 1) + 2
'            VScroll2.Max = M1 + (Combo1.ListIndex + 1) - Ndyi
'            VScroll2 = Yi(2, Combo1.ListIndex + 1) + 2
'            Label1 = "ymax.= "
'        Case Option1(2)
'            Label2 = "xmin.= " & XU(Xs(1, Combo1.ListIndex + 1) + 1)
'            VScroll2.Min = Xs(1, Combo1.ListIndex + 1) + 2
'            VScroll2.Max = L1 + (Combo1.ListIndex + 1) - Ndxs
'            VScroll2 = Xs(2, Combo1.ListIndex + 1) + 2
'            Label1 = "xmax.= "
'        Case Option1(3)
'            Label2 = "ymin.= " & YV(Yd(1, Combo1.ListIndex + 1) + 1)
'            VScroll2.Min = Yd(1, Combo1.ListIndex + 1) + 2
'            VScroll2.Max = M1 + (Combo1.ListIndex + 1) - Ndyd
'            VScroll2 = Yd(2, Combo1.ListIndex + 1) + 2
'            Label1 = "ymax.= "
'    End Select
End Sub

Private Sub Combo1_Click()
  VBCP_Update 18, "Combo1_Click", 1
  caca = True
  VBCP_Update 18, "Combo1_Click", 2
     Select Case caca
        Case Option1(0)
  VBCP_Update 18, "Combo1_Click", 3
            Label2 = "xmin.= " & XU(Xi(1, Combo1.ListIndex + 1) + 1)
  VBCP_Update 18, "Combo1_Click", 4
            tep = Xi(2, Combo1.ListIndex + 1) + 2
  VBCP_Update 18, "Combo1_Click", 5
            VScroll2.Min = Xi(1, Combo1.ListIndex + 1) + 2
  VBCP_Update 18, "Combo1_Click", 6
            VScroll2.Max = L1 + (Combo1.ListIndex + 1) - Ndxi
  VBCP_Update 18, "Combo1_Click", 7
            If tep < VScroll2.Min And VBCP_UpdateIf(18, "Combo1_Click", 8) Then tep = VScroll2.Min
            VScroll2 = tep
  VBCP_Update 18, "Combo1_Click", 9
            Label1 = "xmax.= "
  VBCP_Update 18, "Combo1_Click", 10
        Case Option1(1)
  VBCP_Update 18, "Combo1_Click", 11
            Label2 = "ymin.= " & YV(Yi(1, Combo1.ListIndex + 1) + 1)
  VBCP_Update 18, "Combo1_Click", 12
            tep = Yi(2, Combo1.ListIndex + 1) + 2
  VBCP_Update 18, "Combo1_Click", 13
            VScroll2.Min = Yi(1, Combo1.ListIndex + 1) + 2
  VBCP_Update 18, "Combo1_Click", 14
            VScroll2.Max = M1 + (Combo1.ListIndex + 1) - Ndyi
  VBCP_Update 18, "Combo1_Click", 15
            If tep < VScroll2.Min And VBCP_UpdateIf(18, "Combo1_Click", 16) Then tep = VScroll2.Min
            VScroll2 = tep
  VBCP_Update 18, "Combo1_Click", 17
            Label1 = "ymax.= "
  VBCP_Update 18, "Combo1_Click", 18
        Case Option1(2)
  VBCP_Update 18, "Combo1_Click", 19
            Label2 = "xmin.= " & XU(Xs(1, Combo1.ListIndex + 1) + 1)
  VBCP_Update 18, "Combo1_Click", 20
            tep = Xs(2, Combo1.ListIndex + 1) + 2
  VBCP_Update 18, "Combo1_Click", 21
            VScroll2.Min = Xs(1, Combo1.ListIndex + 1) + 2
  VBCP_Update 18, "Combo1_Click", 22
            VScroll2.Max = L1 + (Combo1.ListIndex + 1) - Ndxs
  VBCP_Update 18, "Combo1_Click", 23
            If tep < VScroll2.Min And VBCP_UpdateIf(18, "Combo1_Click", 24) Then tep = VScroll2.Min
            VScroll2 = tep
  VBCP_Update 18, "Combo1_Click", 25
            Label1 = "xmax.= "
  VBCP_Update 18, "Combo1_Click", 26
        Case Option1(3)
  VBCP_Update 18, "Combo1_Click", 27
            Label2 = "ymin.= " & YV(Yd(1, Combo1.ListIndex + 1) + 1)
  VBCP_Update 18, "Combo1_Click", 28
            tep = Yd(2, Combo1.ListIndex + 1) + 2
  VBCP_Update 18, "Combo1_Click", 29
            VScroll2.Min = Yd(1, Combo1.ListIndex + 1) + 2
  VBCP_Update 18, "Combo1_Click", 30
            VScroll2.Max = M1 + (Combo1.ListIndex + 1) - Ndyd
  VBCP_Update 18, "Combo1_Click", 31
            If tep < VScroll2.Min And VBCP_UpdateIf(18, "Combo1_Click", 32) Then tep = VScroll2.Min
            VScroll2 = tep
  VBCP_Update 18, "Combo1_Click", 33
            Label1 = "ymax.= "
  VBCP_Update 18, "Combo1_Click", 34
    End Select
  

End Sub


Private Sub Command2_Click()
  VBCP_Update 18, "Command2_Click", 1
    Me.Hide
  VBCP_Update 18, "Command2_Click", 2
End Sub

Private Sub Form_Activate()
  VBCP_Update 18, "Form_Activate", 1
   For kk = 3 To 0 Step -1
  VBCP_Update 18, "Form_Activate", 2
        Option1(kk).Enabled = Not (FCF.Check2(kk).Value = vbChecked) And Val(FCF.Text1(kk)) > 1
  VBCP_Update 18, "Form_Activate", 3
        Option1(kk) = Not (FCF.Check2(kk).Value = vbChecked) And Val(FCF.Text1(kk)) > 1
  VBCP_Update 18, "Form_Activate", 4
        
    Next kk
  
End Sub

Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)
  VBCP_Update 18, "Form_QueryUnload", 1
Cancel = True
  VBCP_Update 18, "Form_QueryUnload", 2
End Sub

Private Sub Option1_Click(Index As Integer)
  VBCP_Update 18, "Option1_Click", 1
     Combo1.Clear
  VBCP_Update 18, "Option1_Click", 2
    For J = 0 To 3
  VBCP_Update 18, "Option1_Click", 3
        If Option1(J) And VBCP_UpdateIf(18, "Option1_Click", 4) Then
            Select Case (J)
                Case 0
  VBCP_Update 18, "Option1_Click", 5
                    For I = 1 To Ndxi - 1
  VBCP_Update 18, "Option1_Click", 6
                        Combo1.AddItem (I)
  VBCP_Update 18, "Option1_Click", 7
                        
                        
                    Next I
                    Combo1.ListIndex = 0
  VBCP_Update 18, "Option1_Click", 8
                Case 1
  VBCP_Update 18, "Option1_Click", 9
                    For I = 1 To Ndyi - 1
  VBCP_Update 18, "Option1_Click", 10
                        Combo1.AddItem (I)
  VBCP_Update 18, "Option1_Click", 11
                        
                        
                    Next I
                    Combo1.ListIndex = 0
  VBCP_Update 18, "Option1_Click", 12
                Case 2
  VBCP_Update 18, "Option1_Click", 13
                    For I = 1 To Ndxs - 1
  VBCP_Update 18, "Option1_Click", 14
                       Combo1.AddItem (I)
  VBCP_Update 18, "Option1_Click", 15
                      
                       
                    Next I
                    Combo1.ListIndex = 0
  VBCP_Update 18, "Option1_Click", 16
                Case 3
  VBCP_Update 18, "Option1_Click", 17
                    For I = 1 To Ndyd - 1
  VBCP_Update 18, "Option1_Click", 18
                        Combo1.AddItem (I)
  VBCP_Update 18, "Option1_Click", 19
                        
                        
                    Next I
                    Combo1.ListIndex = 0
  VBCP_Update 18, "Option1_Click", 20
            End Select
    End If
Next J
  
End Sub

Private Sub VScroll2_Change()
  VBCP_Update 18, "VScroll2_Change", 1
    caca = True
  VBCP_Update 18, "VScroll2_Change", 2
    Select Case caca
        Case Option1(0)
  VBCP_Update 18, "VScroll2_Change", 3
            Label2 = "xmin.= " & XU(Xi(1, Combo1.ListIndex + 1) + 1)
  VBCP_Update 18, "VScroll2_Change", 4
            Text2 = XU(VScroll2)
  VBCP_Update 18, "VScroll2_Change", 5
            Xi(2, Combo1.ListIndex + 1) = VScroll2 - 2
  VBCP_Update 18, "VScroll2_Change", 6
            Xi(1, Combo1.ListIndex + 2) = VScroll2 - 1
  VBCP_Update 18, "VScroll2_Change", 7

        Case Option1(1)
  VBCP_Update 18, "VScroll2_Change", 8
            Label2 = "ymin.= " & YV(Yi(1, Combo1.ListIndex + 1) + 1)
  VBCP_Update 18, "VScroll2_Change", 9
            Text2 = YV(VScroll2)
  VBCP_Update 18, "VScroll2_Change", 10
            Yi(2, Combo1.ListIndex + 1) = VScroll2 - 2
  VBCP_Update 18, "VScroll2_Change", 11
            Yi(1, Combo1.ListIndex + 2) = VScroll2 - 1
  VBCP_Update 18, "VScroll2_Change", 12

        Case Option1(2)
  VBCP_Update 18, "VScroll2_Change", 13
            Label2 = "xmin.= " & XU(Xs(1, Combo1.ListIndex + 1) + 1)
  VBCP_Update 18, "VScroll2_Change", 14
            Text2 = XU(VScroll2)
  VBCP_Update 18, "VScroll2_Change", 15
            Xs(2, Combo1.ListIndex + 1) = VScroll2 - 2
  VBCP_Update 18, "VScroll2_Change", 16
            Xs(1, Combo1.ListIndex + 2) = VScroll2 - 1
  VBCP_Update 18, "VScroll2_Change", 17

        Case Option1(3)
  VBCP_Update 18, "VScroll2_Change", 18
            Label2 = "ymin.= " & YV(Yd(1, Combo1.ListIndex + 1) + 1)
  VBCP_Update 18, "VScroll2_Change", 19
            Label1 = "ymax.= "
  VBCP_Update 18, "VScroll2_Change", 20
            Text2 = YV(VScroll2)
  VBCP_Update 18, "VScroll2_Change", 21
            Yd(2, Combo1.ListIndex + 1) = VScroll2 - 2
  VBCP_Update 18, "VScroll2_Change", 22
            Yd(1, Combo1.ListIndex + 2) = VScroll2 - 1
  VBCP_Update 18, "VScroll2_Change", 23

    End Select

End Sub


