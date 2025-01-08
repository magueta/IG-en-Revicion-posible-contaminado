Attribute VB_Name = "Variables"
DefSng A-Z
'Declaraciones de las variables punto flotante de precición simple
Public T(), V(), U(), RHO(), CP(), GBX(), _
          GBY(), rK(), Be(), TSp(), TSc()
Public Xl As Single, Yl As Single
Public X() As Single, Y() As Single
Public XU() As Single, YV() As Single
Public XDif() As Single, YDif() As Single
Public XCV() As Single, YCV() As Single
Public S() As Single, Sx() As Single, Sy() As Single
Public Dx() As Single, Dy() As Single
Public PowX() As Single, PowY() As Single
Public EscalaXY As Single, Pro(4) As Single
Public R1 As Single
Public Txs() As Single, Txi() As Single, Tyd() As Single, Tyi() As Single
Public Vxs() As Single, Vxi() As Single, Uyd() As Single, Uyi() As Single
Public Uxs() As Single, Uxi() As Single, Vyd() As Single, Vyi() As Single
Public VI(2) As Single, Relax(3) As Single
Public TFE() As Single, Gx As Single, Gy As Single, Beta As Single, Er() As Single
'Declaraciones de las variables enteras largas
Public M1 As Long, L1 As Long
Public M2 As Long, L2 As Long
Public M3 As Long, L3 As Long
Public L() As Long, M() As Long
Public LL() As Long, MM() As Long
Public Ns As Long, Nss As Long
Public Ndx As Long, Ndy As Long
Public Tv As Long
Public Tmx() As Long, Tmy() As Long
Public I As Long, J As Long, K As Long
Public Mode As Long, Simetria As Long
Public Ndxi As Long, Ndxs As Long, Ndyi As Long, Ndyd As Long
Public Xi() As Long, Xs() As Long, Yi() As Long, Yd() As Long

Public NTime(4) As Long
'Declaraciones de las variables alfanumericas
Public BloqX() As String, BloqY() As String
'Declaraciones de las variables logicas
Public Tb As Boolean, Bl As Boolean
Public LE As Boolean, LM As Boolean, LG As Boolean
'Declaraciones de las constantes
Const NA = "0", Mmax = 50, Nmax = 50




