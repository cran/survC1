C---------------------------------------------     
C     cp.unoCWv1
C---------------------------------------------     
      subroutine unoCW(n, time, status, weight, wtstar, rs, rsstar, rex,
     1  Dhat, Wa, Wb, Wg,
     1  WK1, WK3A,WK3B,USEP,USEPX)

      integer n, i, j
      integer status(n)
      double precision weight(n), wtstar(n), rex(n), Dhat
      double precision Wa, Wb, Wg, USEP, USEPX, WK1, WK2, WK3A, WK3B
      integer time(n), rs(n), rsstar(n)

      USEP=0
      USEPX=0
      WK1=0
      WK2=0
      WK3A=0
      WK3B=0

      DO i = 1, n
        DO j = 1, n

           IF (time(i) .LT. time(j)) THEN
              IF (status(i).EQ.1) THEN

                 USEP =USEP +weight(i)
                 USEPX=USEPX+wtstar(i)

                 IF (rs(i) .GT. rs(j)) THEN 
                    WK1=WK1+(1-Dhat)*weight(i)*rex(i)*rex(j) 
                    WK2=WK2-weight(i)
                    WK3A=WK3A+(1-Dhat)*wtstar(i)
                    WK3B=WK3B+(1-Dhat)*weight(i)
                 END IF
                 
                 IF (rs(i) .EQ. rs(j)) THEN 
                    WK1=WK1+(0.5-Dhat)*weight(i)*rex(i)*rex(j) 
                    WK2=WK2-weight(i)/2
                    WK3A=WK3A+(0.5-Dhat)*wtstar(i)
                    WK3B=WK3B+(0.5-Dhat)*weight(i)
                 END IF

                 IF (rs(i) .LT. rs(j)) THEN 
                    WK1=WK1-Dhat*weight(i)*rex(i)*rex(j) 
                    WK3A=WK3A-Dhat*wtstar(i)
                    WK3B=WK3B-Dhat*weight(i)
                 END IF

                 IF (rsstar(i) .GT. rsstar(j)) THEN 
                    WK2=WK2+weight(i)
                 END IF 
                 IF (rsstar(i) .EQ. rsstar(j)) THEN 
                    WK2=WK2+weight(i)/2
                 END IF

              END IF
           End IF

        END DO 
      END DO
      Wa=REAL(WK1)/USEP 
      Wb=REAL(WK2)/USEP 
      Wg=REAL(WK3A)/USEPX - REAL(WK3B)/USEP 

      RETURN
      END

C---------------------------------------------     
C     cp. unoU2Pv1.f
C---------------------------------------------     
      subroutine unoU2P(n, p, A, B, OUT, UP)

      integer n, i, j, k, p, UP
      double precision B(n), A(n,p), OUT(p) 
      
      UP=0
      DO i = 1, n-1
        DO j = i+1, n
          UP=UP+1
          DO k = 1, p
             OUT(k)=OUT(k) + (A(i,k)+A(j,k))*B(i)*B(j)/2 
          END DO 
        END DO 
      END DO

      DO k = 1, p
        OUT(k)=OUT(k)/UP
      END DO

      RETURN
      END



C---------------------------------------------     
C     cp. conc4.f
C---------------------------------------------     
      subroutine conc(n, time, status, rs, weight, hfwt, WK1, CSTAT)

      integer n, i, j
      integer time(n), rs(n), status(n)
      double precision CSTAT, weight(n), hfwt(n), WK1, USEP

      USEP=0
      WK1=0
      DO i = 1, n
        DO j = 1, n
           IF (time(i) .LT. time(j)) THEN
              IF (status(i).EQ.1) THEN
                 USEP=USEP+weight(i)
                 IF (rs(i) .GT. rs(j)) THEN 
                    WK1=WK1+weight(i) 
                 END IF 
                 IF (rs(i) .EQ. rs(j)) THEN 
                    WK1=WK1+hfwt(i) 
                 END IF
              END IF
           End IF
        END DO 
      END DO

      CSTAT=REAL(WK1)/USEP 

      RETURN
      END

