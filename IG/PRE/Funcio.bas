Attribute VB_Name = "Funciones"
Function DL(Vs, VI, Ex1, Ex2, XX)
  VBCP_Update 3, "DL", 1
    Static M, N
    M = (Vs - VI) / (Ex2 - Ex1)
  VBCP_Update 3, "DL", 2
    N = VI - Ex1 * M
  VBCP_Update 3, "DL", 3
    DL = M * XX + N
  VBCP_Update 3, "DL", 4
End Function

Function DP(VExt As Single, VCent As Single, Ex1 _
                   As Single, Ex2 As Single, XX As Single) As Single
  VBCP_Update 3, "DP", 5
      Static AA As Single, BB As Single, GG As Single
      AA = 4 * (VExt - VCent) / ((Ex2 - Ex1) * (Ex2 - Ex1))
  VBCP_Update 3, "DP", 6
      BB = (VExt - VCent) * 2 / (Ex2 - Ex1) _
               - (3 * Ex2 + Ex1) / 2 * AA
  VBCP_Update 3, "DP", 7
      GG = VCent - (((Ex1 + Ex2)) * ((Ex1 + Ex2)) * AA / 4 _
              + ((Ex2 + Ex1) / 2 * BB))
  VBCP_Update 3, "DP", 8
      DP = AA * XX * XX + BB * XX + GG
End Function
Function Maximo(A, B)
  VBCP_Update 3, "Maximo", 1
    If A > B And VBCP_UpdateIf(3, "Maximo", 2) Then Maximo = A Else Maximo = B
End Function

