       IDENTIFICATION DIVISION.
        PROGRAM-ID.  bijlkies.
      *    29 DEC 97
       AUTHOR.   EGC BROEK.
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  RMCOBOL-85.
       OBJECT-COMPUTER.  RMCOBOL-85.


       DATA DIVISION.



       WORKING-STORAGE SECTION.
        01      tabwaarde.
                02 mtab         value x'09'     pic x.

        01     standard-control.
               02 pic x(33) value

               "high,FCOLOR=white,low,BCOLOR=cyan".

        01      st-control.
                02 pic x(17) value "low,fcolor=green".

       01  display-group usage display.
           02 x            pic x.
           02 xx           pic xx.
           02 present      pic x value "F".

        77      datum                     pic 9(6).
        77      dmj                       pic 9(6).

        77      exc-num                   pic 99.
        77      jrperiode                 pic 9999.
      /
       PROCEDURE DIVISION.
       HOOFD SECTION.

        AANVANG.
                display space line 1 position 1
                control STANDARD-CONTROL

                erase eos.

           accept datum from date end-accept.
           accept datum from date end-accept.
           string datum(1:4) delimited size into jrperiode.

       KIESPROGRAM.
           display space line 1 position 1 erase eos.

           display "Faktuurregels" line 3 position 30.
           display "Invoeren        =  I" line 5 position 30.
           display "Opvragen        =  O" line 6 position 30.
           display "Wijzigen        =  W" line 7 position 30.
           display "Fakturen 0%     =  N" line 9 position 30.
           display "Fakturen 6%     =  F" line 10 position 30.
           display "Fakturen 19.0%  =  Z" line 11 position 30.
           display "Verwijderen     =  V" line 13 position 30.
           display "Klantgegevens   =  K" line 14 position 30.
           display "Historie 0%     =  M" line 16 position 30.
           display "Historie 6%     =  L" line 17 position 30.
           display "Historie 19.0%  =  H" line 18 position 30.
           display "Print faktuur uit historie  0% = R"
           line 20 position 30.
           display "Print faktuur uit historie  6% = P"
           line 21 position 30.
           display "Print faktuur uit historie 19% = Q"
           line 22 position 30.

           display "Alt + F4  =  Einde" line 25 position 30.

           accept x line 0 position 0.


           if x = "I" or "i" call program "bijlinb.cob" end-if.
           if x = "O" or "o" call program "bijlopv.cob" end-if.
           if x = "F" or "f" call program "bijlfkt1.cob" end-if.
           if x = "V" or "v" call program "bijlvwd.cob" end-if.
           if x = "W" or "w" call program "bijlwyz.cob" end-if.
           if x = "K" or "k" call program "bijlklt.cob" end-if.
           if x = "L" or "l" call program "bijlhis.cob" end-if.
           if x = "Z" or "z" call program "bijlfktp.cob" end-if.
           if x = "H" or "h" call program "bijlhish.cob" end-if.
           if x = "P" or "p" call program "bijlfkh0.cob" end-if.
           if x = "Q" or "q" call program "bijlfkh5.cob" end-if.
           if x = "N" or "n" call program "bijlfktb.cob" end-if.
           if x = "M" or "m" call program "bijlhisn.cob" end-if.
           if x = "R" or "r" call program "bijlfkh8.cob" end-if.

           go to kiesprogram.





       STOPPROGRAM.
           display "EOP" line 25 position 1.

           STOP RUN.

