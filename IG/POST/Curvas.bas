Attribute VB_Name = "Módulo1"
DefSng A-Z
Public Type Vec
    Xb As Single
    Yb As Single
    Xc As Single
    Yc As Single
End Type
Public Type Ca
    InS As Integer
    InI As Integer
    InN As Integer
End Type

Public rK(), Qx(), Qy(), QQx(), QQy(), Qi(), QC() As Ca
Public T(), A(3, 3), B(), x(), y(), R1, F(), _
Xu(), Yv(), Xmax, Xmin, Ymax, Ymin, Xl, Yl, Km
Public P(), Pc(), u(), v(), S(), Ns, Solve As String, uN(), vN() _
, Graf(), Mavg, Mivg, MV()
Public Xgraf(), Ygraf()

Public M1 As Integer, L1 As Integer, _
Nl As Integer, Mode As Integer, M2 As Integer, M3 As _
Integer, L2 As Integer, L3 As Integer

Public Ll As Boolean

Public Velocidad() As Vec


