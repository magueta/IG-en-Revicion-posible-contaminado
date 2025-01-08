Attribute VB_Name = "Rutinas"


Sub ObtenerE()
  VBCP_Update 16, "ObtenerE", 1
        ReDim Preserve Xi(2, Ndxi), Yi(2, Ndyi), _
                                Xs(2, Ndxs), Yd(2, Ndyd)
  VBCP_Update 16, "ObtenerE", 2
        ReDim Preserve Txi(3, Ndxi), Txs(3, Ndxs), _
                                Tyi(3, Ndyi), Tyd(3, Ndyd)
  VBCP_Update 16, "ObtenerE", 3
        ReDim Preserve Vxi(3, Ndxi), Vxs(3, Ndxs), _
                                Uyi(3, Ndyi), Uyd(3, Ndyd)
  VBCP_Update 16, "ObtenerE", 4
        ReDim Preserve Uxi(3, Ndxi), Uxs(3, Ndxs), _
                                Vyi(3, Ndyi), Vyd(3, Ndyd)
  VBCP_Update 16, "ObtenerE", 5
        For I = 1 To Ndxi
            Txi(1, I) = 0: Txi(2, I) = 0: Txi(3, I) = 0
  VBCP_Update 16, "ObtenerE", 6
            Vxi(1, I) = 2: Vxi(2, I) = 0: Vxi(3, I) = 0
  VBCP_Update 16, "ObtenerE", 7
            Uxi(1, I) = 2: Uxi(2, I) = 0: Uxi(3, I) = 0
  VBCP_Update 16, "ObtenerE", 8
        Next I
        For I = 1 To Ndxs
  VBCP_Update 16, "ObtenerE", 9
            Txs(1, I) = 0: Txs(2, I) = 0: Txs(3, I) = 0
  VBCP_Update 16, "ObtenerE", 10
            Vxs(1, I) = 2: Vxs(2, I) = 0: Vxs(3, I) = 0
  VBCP_Update 16, "ObtenerE", 11
            Uxs(1, I) = 2: Uxs(2, I) = 0: Uxs(3, I) = 0
  VBCP_Update 16, "ObtenerE", 12
        Next I
        For I = 1 To Ndyi
  VBCP_Update 16, "ObtenerE", 13
            Tyi(1, I) = 0: Tyi(2, I) = 0: Tyi(3, I) = 0
  VBCP_Update 16, "ObtenerE", 14
            Uyi(1, I) = 2: Uyi(2, I) = 0: Uyi(3, I) = 0
  VBCP_Update 16, "ObtenerE", 15
            Vyi(1, I) = 2: Vyi(2, I) = 0: Vyi(3, I) = 0
  VBCP_Update 16, "ObtenerE", 16
        Next I
        For I = 1 To Ndyd
  VBCP_Update 16, "ObtenerE", 17
            Tyd(1, I) = 0: Tyd(2, I) = 0: Tyd(3, I) = 0
  VBCP_Update 16, "ObtenerE", 18
            Uyd(1, I) = 2: Uyd(2, I) = 0: Uyd(3, I) = 0
  VBCP_Update 16, "ObtenerE", 19
            Vyd(1, I) = 2: Vyd(2, I) = 0: Vyd(3, I) = 0
  VBCP_Update 16, "ObtenerE", 20
        Next I
        
        Xi(1, 1) = 1
  VBCP_Update 16, "ObtenerE", 21
        Xi(2, 1) = L3 \ Ndxi
  VBCP_Update 16, "ObtenerE", 22
        For I = 2 To Ndxi
  VBCP_Update 16, "ObtenerE", 23
            Xi(1, I) = Xi(2, I - 1) + 1
  VBCP_Update 16, "ObtenerE", 24
            Xi(2, I) = Xi(1, I) + L3 \ Ndxi - 1
  VBCP_Update 16, "ObtenerE", 25
        Next I
        Xi(2, Ndxi) = L3
  VBCP_Update 16, "ObtenerE", 26
        
        Xs(1, 1) = 1
  VBCP_Update 16, "ObtenerE", 27
        Xs(2, 1) = L3 \ Ndxs
  VBCP_Update 16, "ObtenerE", 28
        For I = 2 To Ndxs
  VBCP_Update 16, "ObtenerE", 29
            Xs(1, I) = Xs(2, I - 1) + 1
  VBCP_Update 16, "ObtenerE", 30
            Xs(2, I) = Xs(1, I) + L3 \ Ndxs - 1
  VBCP_Update 16, "ObtenerE", 31
        Next I
        Xs(2, Ndxs) = L3
  VBCP_Update 16, "ObtenerE", 32
        
        Yi(1, 1) = 1
  VBCP_Update 16, "ObtenerE", 33
        Yi(2, 1) = M3 \ Ndyi
  VBCP_Update 16, "ObtenerE", 34
        For I = 2 To Ndyi
  VBCP_Update 16, "ObtenerE", 35
            Yi(1, I) = Yi(2, I - 1) + 1
  VBCP_Update 16, "ObtenerE", 36
            Yi(2, I) = Yi(1, I) + M3 \ Ndyi - 1
  VBCP_Update 16, "ObtenerE", 37
        Next I
        Yi(2, Ndyi) = M3
  VBCP_Update 16, "ObtenerE", 38
        
        Yd(1, 1) = 1
  VBCP_Update 16, "ObtenerE", 39
        Yd(2, 1) = M3 \ Ndyd
  VBCP_Update 16, "ObtenerE", 40
        For I = 2 To Ndyd
  VBCP_Update 16, "ObtenerE", 41
            Yd(1, I) = Yd(2, I - 1) + 1
  VBCP_Update 16, "ObtenerE", 42
            Yd(2, I) = Yd(1, I) + M3 \ Ndyd - 1
  VBCP_Update 16, "ObtenerE", 43
        Next I
        Yd(2, Ndyd) = M3
  VBCP_Update 16, "ObtenerE", 44
        
        For J = 0 To 3
  VBCP_Update 16, "ObtenerE", 45
            If FCF.Check2(J) = vbChecked And VBCP_UpdateIf(16, "ObtenerE", 46) Then
                Select Case J
                    Case 0
  VBCP_Update 16, "ObtenerE", 47
                        Xi(1, 1) = 1
  VBCP_Update 16, "ObtenerE", 48
                        Xi(2, 1) = LL(1) - 2
  VBCP_Update 16, "ObtenerE", 49
                        For I = 2 To Ndxi
  VBCP_Update 16, "ObtenerE", 50
                            Xi(1, I) = Xi(2, I - 1) + 1
  VBCP_Update 16, "ObtenerE", 51
                            Xi(2, I) = LL(I) - 2
  VBCP_Update 16, "ObtenerE", 52
                        Next I
                        Xi(2, Ndxi) = L3
  VBCP_Update 16, "ObtenerE", 53
                    Case 1
  VBCP_Update 16, "ObtenerE", 54
                        Yi(1, 1) = 1
  VBCP_Update 16, "ObtenerE", 55
                        Yi(2, 1) = MM(1) - 2
  VBCP_Update 16, "ObtenerE", 56
                        For I = 2 To Ndyi
  VBCP_Update 16, "ObtenerE", 57
                            Yi(1, I) = Yi(2, I - 1) + 1
  VBCP_Update 16, "ObtenerE", 58
                            Yi(2, I) = MM(I) - 2
  VBCP_Update 16, "ObtenerE", 59
                        Next I
                        Yi(2, Ndyi) = M3
  VBCP_Update 16, "ObtenerE", 60
                    Case 2
  VBCP_Update 16, "ObtenerE", 61
                        Xs(1, 1) = 1
  VBCP_Update 16, "ObtenerE", 62
                        Xs(2, 1) = LL(1) - 2
  VBCP_Update 16, "ObtenerE", 63
                        For I = 2 To Ndxs
  VBCP_Update 16, "ObtenerE", 64
                            Xs(1, I) = Xs(2, I - 1) + 1
  VBCP_Update 16, "ObtenerE", 65
                            Xs(2, I) = LL(I) - 2
  VBCP_Update 16, "ObtenerE", 66
                        Next I
                        Xs(2, Ndxs) = L3
  VBCP_Update 16, "ObtenerE", 67
                    Case 3
  VBCP_Update 16, "ObtenerE", 68
                        Yd(1, 1) = 1
  VBCP_Update 16, "ObtenerE", 69
                        Yd(2, 1) = MM(1) - 2
  VBCP_Update 16, "ObtenerE", 70
                        For I = 2 To Ndyd
  VBCP_Update 16, "ObtenerE", 71
                            Yd(1, I) = Yd(2, I - 1) + 1
  VBCP_Update 16, "ObtenerE", 72
                            Yd(2, I) = MM(I) - 2
  VBCP_Update 16, "ObtenerE", 73
                        Next I
                        Yd(2, Ndyd) = M3
  VBCP_Update 16, "ObtenerE", 74
                End Select
            End If
        Next J
End Sub

Sub Cadicio()
  VBCP_Update 16, "Cadicio", 1
    L2 = L1 - 1
  VBCP_Update 16, "Cadicio", 2
    M2 = M1 - 1
  VBCP_Update 16, "Cadicio", 3
    L3 = L2 - 1
  VBCP_Update 16, "Cadicio", 4
    M3 = M2 - 1
  VBCP_Update 16, "Cadicio", 5
    X(L1) = Xl
  VBCP_Update 16, "Cadicio", 6
    Y(M1) = Yl
  VBCP_Update 16, "Cadicio", 7
    For I = 2 To L2
  VBCP_Update 16, "Cadicio", 8
        X(I) = (XU(I + 1) + XU(I)) / 2
  VBCP_Update 16, "Cadicio", 9
        XCV(I) = -XU(I) + XU(I + 1)
  VBCP_Update 16, "Cadicio", 10
    Next I
    For J = 2 To M2
  VBCP_Update 16, "Cadicio", 11
        Y(J) = (YV(J + 1) + YV(J)) / 2
  VBCP_Update 16, "Cadicio", 12
        YCV(J) = -YV(J) + YV(J + 1)
  VBCP_Update 16, "Cadicio", 13
    Next J
    For I = 2 To L1
  VBCP_Update 16, "Cadicio", 14
        XDif(I) = X(I) - X(I - 1)
  VBCP_Update 16, "Cadicio", 15
    Next I
    For J = 2 To M1
  VBCP_Update 16, "Cadicio", 16
        YDif(J) = Y(J) - Y(J - 1)
  VBCP_Update 16, "Cadicio", 17
    Next J
    
End Sub

Sub Expone(TCoor As Boolean, Bloq As Long, Pow As Single, TT As Long)
  VBCP_Update 16, "Expone", 1
    Static Ll1 As Single, Mm1 As Single, Xl2 As Single, Yl2 As Single, _
           Ik As Long, Jk As Long, Frac As Single
  VBCP_Update 16, "Expone", 2
    Select Case TCoor
        
        Case True
  VBCP_Update 16, "Expone", 3
            Select Case TT
                Case 1
  VBCP_Update 16, "Expone", 4
                    Pow = 1
  VBCP_Update 16, "Expone", 5
                    XU(LL(Bloq - 1)) = Dx(0, Bloq)
  VBCP_Update 16, "Expone", 6
                    XU(LL(Bloq)) = Dx(0, Bloq) + Dx(1, Bloq)
  VBCP_Update 16, "Expone", 7
                    Ll1 = L(Bloq) / 2
  VBCP_Update 16, "Expone", 8
                    Xl2 = Dx(1, Bloq) / 2
  VBCP_Update 16, "Expone", 9
                    For Ik = 1 To Ll1
  VBCP_Update 16, "Expone", 10
                        Frac = Xl2 * (Ik / Ll1) ^ Pow
  VBCP_Update 16, "Expone", 11
                        XU(Ik + LL(Bloq - 1)) = XU(LL(Bloq - 1)) + Frac
  VBCP_Update 16, "Expone", 12
                        XU(LL(Bloq) - Ik) = XU(LL(Bloq)) - Frac
  VBCP_Update 16, "Expone", 13
                    Next Ik
                
                Case 2
  VBCP_Update 16, "Expone", 14
                    XU(LL(Bloq - 1)) = Dx(0, Bloq)
  VBCP_Update 16, "Expone", 15
                    XU(LL(Bloq)) = Dx(0, Bloq) + Dx(1, Bloq)
  VBCP_Update 16, "Expone", 16
                    Ll1 = L(Bloq) / 2
  VBCP_Update 16, "Expone", 17
                    Xl2 = Dx(1, Bloq) / 2
  VBCP_Update 16, "Expone", 18
                    For Ik = 1 To Ll1
  VBCP_Update 16, "Expone", 19
                        Frac = Xl2 * (Ik / Ll1) ^ Pow
  VBCP_Update 16, "Expone", 20
                        XU(Ik + LL(Bloq - 1)) = XU(LL(Bloq - 1)) + Frac
  VBCP_Update 16, "Expone", 21
                        XU(LL(Bloq) - Ik) = XU(LL(Bloq)) - Frac
  VBCP_Update 16, "Expone", 22
                    Next Ik
                
                Case 3
  VBCP_Update 16, "Expone", 23
                    XU(LL(Bloq - 1)) = Dx(0, Bloq)
  VBCP_Update 16, "Expone", 24
                    XU(LL(Bloq)) = Dx(0, Bloq) + Dx(1, Bloq)
  VBCP_Update 16, "Expone", 25
                    Ll1 = L(Bloq) / 2
  VBCP_Update 16, "Expone", 26
                    Xl2 = Dx(1, Bloq) / 2
  VBCP_Update 16, "Expone", 27
                    For Ik = 1 To Ll1
  VBCP_Update 16, "Expone", 28
                        Frac = Xl2 * (Ik / Ll1) ^ (1 / Pow)
  VBCP_Update 16, "Expone", 29
                        XU(Ik + LL(Bloq - 1)) = XU(LL(Bloq - 1)) + Frac
  VBCP_Update 16, "Expone", 30
                        XU(LL(Bloq) - Ik) = XU(LL(Bloq)) - Frac
  VBCP_Update 16, "Expone", 31
                    Next Ik
                
                Case 4
  VBCP_Update 16, "Expone", 32
                    XU(LL(Bloq - 1)) = Dx(0, Bloq)
  VBCP_Update 16, "Expone", 33
                    XU(LL(Bloq)) = Dx(0, Bloq) + Dx(1, Bloq)
  VBCP_Update 16, "Expone", 34
                    Ll1 = L(Bloq)
  VBCP_Update 16, "Expone", 35
                    Xl2 = Dx(1, Bloq)
  VBCP_Update 16, "Expone", 36
                    For Ik = 1 To Ll1
  VBCP_Update 16, "Expone", 37
                        Frac = Xl2 * (Ik / Ll1) ^ Pow
  VBCP_Update 16, "Expone", 38
                        XU(Ik + LL(Bloq - 1)) = XU(LL(Bloq - 1)) + Frac
  VBCP_Update 16, "Expone", 39
                    Next Ik
                
                Case 5
  VBCP_Update 16, "Expone", 40
                    XU(LL(Bloq - 1)) = Dx(0, Bloq)
  VBCP_Update 16, "Expone", 41
                    XU(LL(Bloq)) = Dx(0, Bloq) + Dx(1, Bloq)
  VBCP_Update 16, "Expone", 42
                    Ll1 = L(Bloq)
  VBCP_Update 16, "Expone", 43
                    Xl2 = Dx(1, Bloq)
  VBCP_Update 16, "Expone", 44
                    For Ik = 1 To Ll1
  VBCP_Update 16, "Expone", 45
                        Frac = Xl2 * (Ik / Ll1) ^ Pow
  VBCP_Update 16, "Expone", 46
                        XU(LL(Bloq) - Ik) = XU(LL(Bloq)) - Frac
  VBCP_Update 16, "Expone", 47
                    Next Ik
            
            End Select
        Case False
  VBCP_Update 16, "Expone", 48
            Select Case TT
                
                Case 1
  VBCP_Update 16, "Expone", 49
                    Pow = 1
  VBCP_Update 16, "Expone", 50
                    YV(MM(Bloq - 1)) = Dy(0, Bloq)
  VBCP_Update 16, "Expone", 51
                    YV(MM(Bloq)) = Dy(0, Bloq) + Dy(1, Bloq)
  VBCP_Update 16, "Expone", 52
                    Mm1 = M(Bloq) / 2
  VBCP_Update 16, "Expone", 53
                    Yl2 = Dy(1, Bloq) / 2
  VBCP_Update 16, "Expone", 54
                    For Jk = 1 To Mm1
  VBCP_Update 16, "Expone", 55
                        Frac = Yl2 * (Jk / Mm1) ^ Pow
  VBCP_Update 16, "Expone", 56
                        YV(Jk + MM(Bloq - 1)) = YV(MM(Bloq - 1)) + Frac
  VBCP_Update 16, "Expone", 57
                        YV(MM(Bloq) - Jk) = YV(MM(Bloq)) - Frac
  VBCP_Update 16, "Expone", 58
                    Next Jk
                
                Case 2
  VBCP_Update 16, "Expone", 59
                    YV(MM(Bloq - 1)) = Dy(0, Bloq)
  VBCP_Update 16, "Expone", 60
                    YV(MM(Bloq)) = Dy(0, Bloq) + Dy(1, Bloq)
  VBCP_Update 16, "Expone", 61
                    Mm1 = M(Bloq) / 2
  VBCP_Update 16, "Expone", 62
                    Yl2 = Dy(1, Bloq) / 2
  VBCP_Update 16, "Expone", 63
                    For Jk = 1 To Mm1
  VBCP_Update 16, "Expone", 64
                        Frac = Yl2 * (Jk / Mm1) ^ Pow
  VBCP_Update 16, "Expone", 65
                        YV(Jk + MM(Bloq - 1)) = YV(MM(Bloq - 1)) + Frac
  VBCP_Update 16, "Expone", 66
                        YV(MM(Bloq) - Jk) = YV(MM(Bloq)) - Frac
  VBCP_Update 16, "Expone", 67
                    Next Jk
                
                Case 3
  VBCP_Update 16, "Expone", 68
                    YV(MM(Bloq - 1)) = Dy(0, Bloq)
  VBCP_Update 16, "Expone", 69
                    YV(MM(Bloq)) = Dy(0, Bloq) + Dy(1, Bloq)
  VBCP_Update 16, "Expone", 70
                    Mm1 = M(Bloq) / 2
  VBCP_Update 16, "Expone", 71
                    Yl2 = Dy(1, Bloq) / 2
  VBCP_Update 16, "Expone", 72
                    For Jk = 1 To Mm1
  VBCP_Update 16, "Expone", 73
                        Frac = Yl2 * (Jk / Mm1) ^ (1 / Pow)
  VBCP_Update 16, "Expone", 74
                        YV(Jk + MM(Bloq - 1)) = YV(MM(Bloq - 1)) + Frac
  VBCP_Update 16, "Expone", 75
                        YV(MM(Bloq) - Jk) = YV(MM(Bloq)) - Frac
  VBCP_Update 16, "Expone", 76
                    Next Jk
                
                Case 4
  VBCP_Update 16, "Expone", 77
                    YV(MM(Bloq - 1)) = Dy(0, Bloq)
  VBCP_Update 16, "Expone", 78
                    YV(MM(Bloq)) = Dy(0, Bloq) + Dy(1, Bloq)
  VBCP_Update 16, "Expone", 79
                    Mm1 = M(Bloq)
  VBCP_Update 16, "Expone", 80
                    Yl2 = Dy(1, Bloq)
  VBCP_Update 16, "Expone", 81
                    For Jk = 1 To Mm1
  VBCP_Update 16, "Expone", 82
                        Frac = Yl2 * (Jk / Mm1) ^ Pow
  VBCP_Update 16, "Expone", 83
                        YV(Jk + MM(Bloq - 1)) = YV(MM(Bloq - 1)) + Frac
  VBCP_Update 16, "Expone", 84
                    Next Jk
                
                Case 5
  VBCP_Update 16, "Expone", 85
                    YV(MM(Bloq - 1)) = Dy(0, Bloq)
  VBCP_Update 16, "Expone", 86
                    YV(MM(Bloq)) = Dy(0, Bloq) + Dy(1, Bloq)
  VBCP_Update 16, "Expone", 87
                    Mm1 = M(Bloq)
  VBCP_Update 16, "Expone", 88
                    Yl2 = Dy(1, Bloq)
  VBCP_Update 16, "Expone", 89
                    For Jk = 1 To Mm1
  VBCP_Update 16, "Expone", 90
                        Frac = Yl2 * (Jk / Mm1) ^ Pow
  VBCP_Update 16, "Expone", 91
                        YV(MM(Bloq) - Jk) = YV(MM(Bloq)) - Frac
  VBCP_Update 16, "Expone", 92
                    Next Jk
            
            End Select
    
    End Select
End Sub

Sub ObtenerS()
  VBCP_Update 16, "ObtenerS", 1
    Nss = 2 * Ns + 2
  VBCP_Update 16, "ObtenerS", 2
    ReDim Sx(Nss), Sy(Nss)
  VBCP_Update 16, "ObtenerS", 3
    Sx(1) = 0
  VBCP_Update 16, "ObtenerS", 4
    Sy(1) = 0
  VBCP_Update 16, "ObtenerS", 5
    Sx(Nss) = Xl
  VBCP_Update 16, "ObtenerS", 6
    Sy(Nss) = Yl
  VBCP_Update 16, "ObtenerS", 7
    For K = 2 To Nss - 2 Step 2
  VBCP_Update 16, "ObtenerS", 8
        Sx(K) = S(0, K \ 2)
  VBCP_Update 16, "ObtenerS", 9
        Sx(K + 1) = S(2, K \ 2)
  VBCP_Update 16, "ObtenerS", 10
        Sy(K) = S(1, K \ 2)
  VBCP_Update 16, "ObtenerS", 11
        Sy(K + 1) = S(3, K \ 2)
  VBCP_Update 16, "ObtenerS", 12
        
    Next K
    OrdenaS Sx
  VBCP_Update 16, "ObtenerS", 13
    OrdenaS Sy
  VBCP_Update 16, "ObtenerS", 14
    Ndx = 0
  VBCP_Update 16, "ObtenerS", 15
    Ndy = 0
  VBCP_Update 16, "ObtenerS", 16
    For K = 1 To Nss - 1
  VBCP_Update 16, "ObtenerS", 17
        If Sx(K + 1) <> Sx(K) And VBCP_UpdateIf(16, "ObtenerS", 18) Then
            Ndx = Ndx + 1
  VBCP_Update 16, "ObtenerS", 19
            ReDim Preserve Dx(1, Ndx)
  VBCP_Update 16, "ObtenerS", 20
            Dx(1, Ndx) = Sx(K + 1) - Sx(K)
  VBCP_Update 16, "ObtenerS", 21
            Dx(0, Ndx) = Sx(K)
  VBCP_Update 16, "ObtenerS", 22
        End If
    Next K
    For K = 1 To Nss - 1
  VBCP_Update 16, "ObtenerS", 23
        If Sy(K + 1) <> Sy(K) And VBCP_UpdateIf(16, "ObtenerS", 24) Then
            Ndy = Ndy + 1
  VBCP_Update 16, "ObtenerS", 25
            ReDim Preserve Dy(1, Ndy)
  VBCP_Update 16, "ObtenerS", 26
            Dy(1, Ndy) = Sy(K + 1) - Sy(K)
  VBCP_Update 16, "ObtenerS", 27
            Dy(0, Ndy) = Sy(K)
  VBCP_Update 16, "ObtenerS", 28
        End If
    Next K

End Sub

'**********************************************
'Ordenacion por el metodo Shell.
'Hecha por Juan C. Magueta B.
'
'Ass() es la "lista" a ordenar.
'
'
'Metodo Shell de ordenación fue descubierto por Donald Shell
'hace mas de treinta años, nadie sabe como ordena tan rapido.
'
'Este metodo se debe escojer cuando una lista sea medianamente grande.
'
'*********************************************************************
Sub OrdenaS(Ass)
  VBCP_Update 16, "OrdenaS", 1
    Dim NumeroDeEntradas As Long, Increm As Long, JJ As Long, Temp As Single
    NumeroDeEntradas = UBound(Ass)
  VBCP_Update 16, "OrdenaS", 2
    Increm = NumeroDeEntradas \ 2
  VBCP_Update 16, "OrdenaS", 3
    Do Until Increm < 1
  VBCP_Update 16, "OrdenaS", 4
        For II = Increm + 1 To NumeroDeEntradas                 '
  VBCP_Update 16, "OrdenaS", 5
            Temp = Ass(II)                                      '
  VBCP_Update 16, "OrdenaS", 6
            For JJ = II - Increm To 1 Step -Increm              '
  VBCP_Update 16, "OrdenaS", 7
                If Temp >= Ass(JJ) And VBCP_UpdateIf(16, "OrdenaS", 8) Then Exit For                '
                Ass(JJ + Increm) = Ass(JJ)                      '
  VBCP_Update 16, "OrdenaS", 9
            Next JJ                                             '
            Ass(JJ + Increm) = Temp                             '
  VBCP_Update 16, "OrdenaS", 10
        Next II                                                 '
        Increm = Increm \ 2                                     '
  VBCP_Update 16, "OrdenaS", 11
    Loop                                                        ' Fin del Bucle de ordenación
    
End Sub

Sub VCUniforme(TCoor As Boolean, Bloq)
  VBCP_Update 16, "VCUniforme", 1
    Select Case TCoor
        
        Case True
  VBCP_Update 16, "VCUniforme", 2
            XU(LL(Bloq - 1)) = Dx(0, Bloq)
  VBCP_Update 16, "VCUniforme", 3
            XU(LL(Bloq)) = Dx(0, Bloq) + Dx(1, Bloq)
  VBCP_Update 16, "VCUniforme", 4
            For I = LL(Bloq - 1) + 1 To LL(Bloq) - 1
  VBCP_Update 16, "VCUniforme", 5
                XU(I) = (I - LL(Bloq - 1)) / (L(Bloq)) * Dx(1, Bloq) + XU(LL(Bloq - 1))
  VBCP_Update 16, "VCUniforme", 6
            Next I
        
        Case False
  VBCP_Update 16, "VCUniforme", 7
            YV(MM(Bloq - 1)) = Dy(0, Bloq)
  VBCP_Update 16, "VCUniforme", 8
            YV(MM(Bloq)) = Dy(0, Bloq) + Dy(1, Bloq)
  VBCP_Update 16, "VCUniforme", 9
            For I = MM(Bloq - 1) + 1 To MM(Bloq) - 1
  VBCP_Update 16, "VCUniforme", 10
                YV(I) = (I - MM(Bloq - 1)) / (M(Bloq)) * Dy(1, Bloq) + YV(MM(Bloq - 1))
  VBCP_Update 16, "VCUniforme", 11
            Next I
    End Select

End Sub



