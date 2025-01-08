VERSION 5.00
Object = "{5E9E78A0-531B-11CF-91F6-C2863C385E30}#1.0#0"; "MSFLXGRD.OCX"
Begin VB.Form VCalor 
   Caption         =   "Valores de calores"
   ClientHeight    =   6015
   ClientLeft      =   300
   ClientTop       =   390
   ClientWidth     =   9105
   LinkTopic       =   "Form1"
   ScaleHeight     =   0
   ScaleWidth      =   0
   Begin VB.CommandButton Command2 
      Caption         =   "Volver"
      Height          =   495
      Left            =   840
      TabIndex        =   2
      Top             =   5220
      Width           =   1215
   End
   Begin VB.PictureBox Picture1 
      Appearance      =   0  'Flat
      AutoRedraw      =   -1  'True
      BackColor       =   &H80000005&
      ForeColor       =   &H80000008&
      Height          =   5895
      Left            =   3120
      ScaleHeight     =   391
      ScaleMode       =   3  'Píxel
      ScaleWidth      =   391
      TabIndex        =   1
      Top             =   60
      Width           =   5895
   End
   Begin MSFlexGridLib.MSFlexGrid MSFlexGrid1 
      Height          =   4755
      Left            =   120
      TabIndex        =   0
      Top             =   240
      Width           =   2835
      _ExtentX        =   5001
      _ExtentY        =   8387
      _Version        =   393216
   End
End
Attribute VB_Name = "VCalor"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
