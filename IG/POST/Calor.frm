VERSION 5.00
Begin VB.Form Calor 
   AutoRedraw      =   -1  'True
   Caption         =   "Calculos de Calores del Dominio"
   ClientHeight    =   6120
   ClientLeft      =   585
   ClientTop       =   465
   ClientWidth     =   8610
   ClipControls    =   0   'False
   ControlBox      =   0   'False
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   6120
   ScaleWidth      =   8610
   Begin VB.Frame Frame1 
      Caption         =   "Calor"
      Height          =   1275
      Left            =   240
      TabIndex        =   5
      Top             =   240
      Width           =   2115
      Begin VB.TextBox Text2 
         Height          =   315
         Left            =   840
         TabIndex        =   9
         Top             =   720
         Width           =   1155
      End
      Begin VB.TextBox Text1 
         Height          =   315
         Left            =   840
         TabIndex        =   7
         Top             =   300
         Width           =   1155
      End
      Begin VB.Label Label2 
         Caption         =   "Lado"
         Height          =   195
         Left            =   60
         TabIndex        =   8
         Top             =   780
         Width           =   555
      End
      Begin VB.Label Label1 
         Caption         =   "Ubicación"
         Height          =   195
         Left            =   60
         TabIndex        =   6
         Top             =   360
         Width           =   735
      End
   End
   Begin VB.PictureBox Picture2 
      Appearance      =   0  'Flat
      BackColor       =   &H00000000&
      ForeColor       =   &H80000008&
      Height          =   1815
      Left            =   480
      ScaleHeight     =   1785
      ScaleWidth      =   1785
      TabIndex        =   4
      Top             =   2040
      Width           =   1815
   End
   Begin VB.CommandButton Command2 
      Caption         =   "Volver"
      Height          =   495
      Left            =   600
      TabIndex        =   3
      Top             =   4800
      Width           =   1215
   End
   Begin VB.CommandButton Command1 
      Caption         =   "Valores"
      Height          =   495
      Left            =   600
      TabIndex        =   2
      Top             =   4200
      Width           =   1215
   End
   Begin VB.HScrollBar HScroll1 
      Height          =   195
      Left            =   240
      TabIndex        =   1
      Top             =   1620
      Width           =   2175
   End
   Begin VB.PictureBox Picture1 
      Appearance      =   0  'Flat
      AutoRedraw      =   -1  'True
      BackColor       =   &H80000005&
      ForeColor       =   &H80000008&
      Height          =   5895
      Left            =   2520
      ScaleHeight     =   391
      ScaleMode       =   3  'Píxel
      ScaleWidth      =   391
      TabIndex        =   0
      Top             =   120
      Width           =   5895
   End
End
Attribute VB_Name = "Calor"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
