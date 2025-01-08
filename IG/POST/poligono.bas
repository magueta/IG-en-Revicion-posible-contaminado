Attribute VB_Name = "Poligonos"


Declare Function Polygon Lib "gdi32" (ByVal hdc As Long, lpPoint As Any, ByVal nCount As Long) As Long
Type POINTAPI
        x As Long
        y As Long
End Type

' Para crear puntos del polinomio hace falta que se declaren los arreglos de la forma
' pp (1 to n) as POINTAPI
' y el color tiene que estar dado como su valor numerico.
Sub Poly(Obj As Object, Po() As POINTAPI, Np As Long, Color As Long)

Obj.FillStyle = 0

Obj.FillColor = Color
Obj.ForeColor = Color
Obj.DrawWidth = 1
Obj.DrawStyle = 5
Check = Polygon(Obj.hdc, Po(1), Np)

End Sub


