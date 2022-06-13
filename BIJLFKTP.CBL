
        IDENTIFICATION DIVISION.
        PROGRAM-ID.  bijlfktp.
      *  PROGRAMMA MAAKT DEFINITIEVE FAKTUREN.
      *  FAKTURERING BTW 17.5% (O.A. PRECARIO)
      *  BEVEILIGD BESTANDEN
      *  VERWYDERD FAKTUURREGELS
      *    29 DEC 97
      *    jan 2001  btw van 17,5 naar 19,0
      *    dec 2001 over naar euro / fkt naar 2 exemplaren
      *    mei 2006 aanpassing hp1010 uitlijning, meerdere pagina's

       AUTHOR.   EGC BROEK.
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  RMCOBOL-85.
       OBJECT-COMPUTER.  RMCOBOL-85.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           select artikelfile assign to disk
                                  "c:\cbl\bijl\artfl.zwa"
                                  organization is indexed
                                  access mode is dynamic
                                  record key is artnr
                                  file status is statusartfl.
           select artmutfile assign to disk
                                  "c:\cbl\bijl\artmut.zwa"
                                  organization is indexed
                                  access mode is dynamic
                                  record key is morder.

           select arthisfile assign to disk
                               "c:\cbl\bijl\arthis.zwa"
                               organization is sequential
                               access mode is sequential.

           select fkthisfile assign to disk
                               "c:\cbl\bijl\fkthish.zwa"
                               organization is sequential
                               access mode is sequential.

           select debmutfile assign to disk
                                  "c:\cbl\bijl\debmut.zwa"
                                  organization is sequential
                                  access mode is sequential.
           select debstamfile assign to disk
                                  "c:\cbl\bijl\debstam.zwa"
                                  organization is indexed
                                  access mode is dynamic
                                  record key is debnr.
                select kontrolefile assign to disk
                                "c:\cbl\bijl\kontr.zwa"
                                organization is sequential
                                access mode is sequential.
           select kfktfile assign to disk
                       "c:\cbl\bijl\kfktnr.zwa"
                       organization is sequential
                       access mode is sequential.

                select printfile assign to print "PRINTER"
                        file status is statusprintfl.
                select tekstfile assign to disk
                                "c:\cbl\bijl\tekst.txt"
                                organization is sequential
                                access mode is sequential.
                select tfile assign to disk
                                "c:\cbl\bijl\bijl.txt"
                                organization is sequential
                                access mode is sequential.

                select workhisfile assign to disk.
       DATA DIVISION.
       FILE SECTION.
       FD  artikelfile     DATA RECORD IS artikelrecord.
       01  artikelrecord.
                02 artnr                        pic x(5).
                02 omschryving                  pic x(15).
                02 btwartnr                     pic 99v99.
        FD      artmutfile              data record is artmutrecord.
       01  artmutrecord.
                02 mdeb                  pic x(10).
                02 mdatum                pic x(6).
                02 morder                pic x(4).
                02 martnr                pic x(5).
                02 momschryving          pic x(15).
                02 maantal               pic -999999.99.
                02 mverkoopprys          pic 9(4).99.


       FD  arthisfile              data record is arthisrecord.
       01  arthisrecord.
                02 hdeb                  pic x(10).
                02 hdatum                pic x(6).
                02 horder                pic x(4).
                02 hartnr                pic x(5).
                02 homschryving          pic x(15).
                02 haantal               pic -999999.99.
                02 hverkoopprys          pic 9(4).99.

       FD  fkthisfile              data record is fkthisrecord.
       01  fkthisrecord.
           02 fhdeb                  pic x(10).
           02 fhfktnr                pic x(5).
           02 fhfktdatum             pic x(6).
           02 fhdatum                pic x(6).
           02 fhorder                pic x(4).
           02 fhartnr                pic x(5).
           02 fhomschryving          pic x(15).
           02 fhaantal               pic -999999.99.
           02 fhverkoopprys          pic 9(4).99.

       FD  debstamfile     data record is debstamrecord.
       01  debstamrecord.
           02 debnr                     pic x(10).
           02 debnaam1                  pic x(30).
           02 debnaam2                  pic x(25).
           02 debcode                   pic x(5).
           02 debstraat                 pic x(30).
           02 debstrnr                  pic x(4).
           02 debpostkode               pic x(6).
           02 debpostbus                pic x(6).
           02 debpkpostbus              pic x(6).
           02 debwnplts                 pic x(30).
           02 debland                   pic x(30).
           02 debtlf                    pic x(15).
           02 debfax                    pic x(15).
           02 debbank1                  pic x(9).
           02 debbtwnr                  pic x(14).



       FD  debmutfile      data record debmutrecord.
       01  debmutrecord.
           02 dmdebnr                               pic x(10).
           02 dmartnr                               pic x(5).
           02 dmfktnr                               pic 9(6).
           02 dmdatum                               pic 9(6).
           02 dmperiode                 pic 9(4).
           02 dmtrek                                pic x(5).
           02 dmaantal                  pic s9(6).
           02 dmbedrag                  pic s9(7)v99.

        FD      kontrolefile    data record is kontrolerecord.
        01      kontrolerecord.
                02 korder                   pic 9999.

       FD  kfktfile            data record is kfktrecord.
       01  kfktrecord.
           02  kfktnr          pic 99999.

       FD printfile.
       01 printrecord             pic x(132).

       FD tekstfile       data record is tekstrecord.
       01 tekstrecord                 pic x(87).

       FD tfile           data record is trecord.
       01 trecord.
           02 tdeb                  pic x(10).
           02 tdatum                pic x(6).
           02 torder                pic x(4).
           02 tartnr                pic x(5).
           02 tomschryving          pic x(15).
           02 taantal               pic -999999.99.
           02 tverkoopprys          pic 9(4).99.



        SD      workhisfile.
        01      sortmut.
                02 sdeb                  pic x(10).
                02 sdatum                pic x(6).
                02 sorder                pic x(4).
                02 sartnr                pic x(5).
                02 somschryving          pic x(15).
                02 saantal               pic -999999.99.
                02 sverkoopprys          pic 9(4).99.




       WORKING-STORAGE SECTION.

       01  COMM-DEB                        PIC X(10).

       01      tabwaarde.
                02 mtab         value x'09'     pic x.

       01      tekstmaak.
                02 tm                   pic x(87).

       01  faktuur.
                02 fdeb                  pic x(10).
                02 fdatum                pic x(6).
                02 forder                pic x(4).
                02 fartnr                pic x(5).
                02 fomschryving          pic x(15).
                02 faantal               pic -zzzzz9.99.
                02 fverkoopprys          pic zzz9.99.

       01  detailregel.
               02 filler           pic x(4) value space.
               02 drdatum           pic x(6) value space.
               02 filler           pic x value '/'.
               02 drorder           pic x(4) value space.
               02 filler           pic x(5) value space.
               02 draantal          pic -zzzzz9.99 value zero.
               02 filler           pic x(5) value spaces.
               02 drartnr           pic x(5) value space.
               02 filler           pic x(5) value space.
               02 dromschryving     pic x(15) value space.
               02 filler           pic x(6) value space.
               02 drverkoopprys     pic zzz9.99 value zero.
               02 filler           pic x(5) value space.
               02 drbedrag          pic zzz,zz9.99-.
               02 filler           pic x(5) value spaces.


        77      aantal                    pic s9(6)v99.
        77      amflstat                  pic xx.
        77      artnrin                   pic x(5).
        77      bedrag                    pic s9(6)v99.
        77      bedragbtw                 pic s9(6)v99.
        77      bedragincl                pic s9(6)v99.
        77      dag                       pic xx.

        77      datum                     pic 9(6).
        77      datumvan                  pic 9(6).
        77      datumtot                  pic 9(6).
        77      debnrin                   pic x(10).
        77      detailregeltel            pic 99.
        77      eerste                    pic x.

        77      exc-num                   pic 99.
        77      faktuurnr                 pic 99999.
        77      fktnrhis                  pic 99999.
        77      jaar                      pic xx.
        77      jrperiode                 pic 9999.
        77      keuze                     pic x.
        77      kopietel                  pic 9.
        77      maand                     pic xx.
        77      ordernr                   pic 9999.
        77      orderrgl                  pic 99999.
        77      pregelcontrol             pic 999.
        77      pregeltel                 pic 999.
        77      pagteller                 pic 999.
        77      printregel                pic x(132).
        77      regeltotexcl              pic s9(5)v99.
        77      dbedrag                   pic -zzzzz9.99.
        77      dverkoopprys              pic zzz9.99.
        77      daantal                   pic -zzzzz9.99.
        77      schermtel                 pic 99.
        77      spatie38                  pic x(38) value spaces.
        77      spatie39                  pic x(39) value spaces.
        77      spatie40                  pic x(40) value spaces.
        77      spatie20                  pic x(20) value spaces.
        77      spatie10                  pic x(10) value spaces.
        77      spatie5                   pic x(5) value spaces.
        77      statusartfl               pic x(2).
        77      statusprintfl             pic x(2).
        77      tekst                     pic x(30).
        77      tekstfaktuurnr            pic zzzz9.
        77      teksteen                  pic x.
        77      tekstbedrag               pic -zzz,zz9.99.
        77      tekstbedragbtw            pic -zzz,zz9.99.
        77      tekstbedragincl           pic -zzz,zz9.99.
        77      tekstaantal               pic -zzzzz9.99.
        77      tekstverkoopprys          pic zzz9.99.
        77      teksteuro                 pic -zzzzz9.99.
        77      tel                       pic 99.
        77      teller                    pic 99.
        77      testdeb                   pic x(10).
        77      totaantal                 pic s9(6)v99.
        77      totbedrexcl               pic s9(6)v99.
        77      totbedrexclpr             pic -zzz,zz9.99.
        77      toteuro                   pic s9(5)v99.

        77      verkoopprys               pic 9(4)v99.
        77      x                         pic x.

       PROCEDURE DIVISION.
       DECLARATIVES.
       PRINTERFOUT SECTION.
           use after standard error procedure on printfile.
       PRINTERFOUTMELDING.
           display
           "Printerfout !! Maak printer gereed en druk op een toets"
           line 25 position 1 erase eol.
           accept keuze line 25 position 70 tab.
           write printrecord from printregel
           AFTER ADVANCING 15 LINES end-write.
       END DECLARATIVES.

       HOOFD SECTION.


       AANVANG.
           display space line 1 position 1 erase eos.

           accept datum from date end-accept.
           accept datum from date end-accept.
           move 1 to kopietel.
                move 1 to pregeltel.
                move 1 to detailregeltel.
                move 1 to pagteller.
                move zero to totbedrexcl.
                string datum(1:4) delimited size into jrperiode.
                move "J" to eerste.
       FAKTUUR.
                display SPACE line 1 position 1 erase eos.
                move 2 to schermtel.
                open input kontrolefile.
                read kontrolefile next record end-read.
                move korder to ordernr.
                close kontrolefile.
                open input kfktfile.
                read kfktfile next record end-read.
                move kfktnr to faktuurnr.
                move kfktnr to fktnrhis.
                close kfktfile.
                display "Esc = einde programma" line 25 position 28
                erase eol.
                display "Selectie klant:"
                line 3 position 1 erase eol.
           display
           "================================="
           line 10 position 20.
           display
           "DEFINITIEVE FAKTUUR INCL. 19% BTW WORDT GEMAAKT"
           line 12 position 20.
           display
           "================================="
           line 14 position 20.

       KLANTSELECTIE.
           accept debnrin line 3 position 25 tab
           on exception exc-num if exc-num = 27 go to stopprogram-0
           end-if.
           inspect debnrin converting
           "abcdefghijklmnopqrstuvwxyz" to
           "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
           display debnrin line 3 position 25.
           display "Klant akkoord = J" line 3 position 50.
           accept x line 3 position 70 tab.
           if x = "J" or "j" go to overzet.
           go to klantselectie.

       OVERZET.
                open i-o artmutfile.
                open output tfile.
                open extend arthisfile.

                move debnrin to comm-deb.

       OVERZET0.
                read artmutfile next record at end
                close artmutfile
                close tfile
                close arthisfile
                go to sorteren end-read.
                move artmutrecord to trecord.

                write trecord end-write.

      *  == ARTIKELMUTATIE OVERZETTEN NAAR HISTORIE LEEGMAKEN MUTATIE ==

           if mdeb = debnrin
                move artmutrecord to arthisrecord

                write arthisrecord end-write
                delete artmutfile record end-delete
                end-if.

                go to overzet0.

       SORTEREN.
                sort workhisfile ascending key  sdeb
                                                sorder

                     using tfile giving tfile.

       LEZEN.
                open output printfile.
                open i-o tfile.
                move spaces to testdeb.
                move debnrin to testdeb.
       TEKST0.
                read tfile next record at end

                perform faktuurtotaal
                perform faktuurvoet
                close printfile
                close tfile
                go to stopprogram
                end-read.
                if testdeb = spaces move tdeb to testdeb end-if.
                if debnrin not equal to tdeb go to tekst0 end-if.


       TEKSTOVERBRENG.

                move spaces to faktuur.
                if testdeb not equal tdeb
                move spaces to printregel
                move tdeb to testdeb
                end-if.
                move taantal to aantal.
                move tverkoopprys to verkoopprys.
                compute bedrag = aantal * verkoopprys.
                move bedrag to tekstbedrag.
                add bedrag to totbedrexcl.
                move tverkoopprys to tekstverkoopprys.
                move taantal to tekstaantal.
                add 1 to schermtel.
                if schermtel = 21 move 3 to schermtel.
       AFDRUKKEN.
                move spaces to printregel.
       AFDRUKEERSTE.
                if eerste = "J" move spaces to testdeb
                                move space to eerste
                                perform faktuurhoofd
                                perform faktuurhoofd-00
                                end-if.
           move zeros to daantal dverkoopprys dbedrag.
           move tekstaantal to daantal.
           move tekstverkoopprys to dverkoopprys.
           move tekstbedrag to dbedrag.
           move spaces to printregel.
           move spaces to printregel.

       DETAILREGEL.
      *    === eerste detailregel = 30 totaal 50 detailregels ===
           move zeros to draantal drverkoopprys.
           move spaces to drdatum drorder drartnr dromschryving.

           move dbedrag to drbedrag.
           move dverkoopprys to drverkoopprys.
           move tdatum to drdatum.
           move torder to drorder.
           move daantal to draantal.
           move tomschryving to dromschryving.
           move tartnr to drartnr.

           move spaces to printregel.
           move detailregel to printregel.
           write printrecord from printregel end-write.
           add 1 to pregeltel.
           add 1 to detailregeltel.

      * == 50 LINES PRINTED NEXT PAGE NEEDED.
             if detailregeltel = 50
               move spaces to printregel
               string
               "Einde pagina: " pagteller delimited size into printregel
               write printrecord from printregel after advancing 2 lines
               move spaces to printregel
               write printrecord from printregel
               after advancing 6 lines end-write
               add 1 to pagteller
               perform faktuurhoofd
               perform faktuurhoofd-00
               end-if.
      * aantal te boeken fakturen in histotiebestand.

                if kopietel > 1 go tekst0.
      *          open output fkthisfile.
                open extend fkthisfile.
                move tdeb to fhdeb.
                move fktnrhis to fhfktnr.
                string datum(5:2) datum(3:2) datum(1:2)
                delimited size into fhfktdatum.

                move datum to fhfktdatum.
                move torder to fhorder.
                move tartnr to fhartnr.
                move tomschryving to fhomschryving.
                move taantal to fhaantal.
                move tdatum to fhdatum.
                move tverkoopprys to fhverkoopprys.
                write fkthisrecord end-write.
                close fkthisfile.
                go to tekst0.

        FAKTUURHOOFD.
                move spaces to printregel.
                open input debstamfile.
                move tdeb to debnr.
                read debstamfile record key debnr invalid key
                move spaces to printregel end-read.
                string
                spatie40 spatie20 debnaam1
                delimited size into printregel.
                write printrecord from printregel
                after advancing 15 lines.

                add 15 to pregeltel.
                move spaces to printregel.
                string
                spatie40 spatie20 debnaam2

                delimited size into printregel.
                write printrecord from printregel after advancing
                1 line end-write.
                add 1 to pregeltel.
                move spaces to printregel.
                string
                spatie40 spatie20
                debstraat "  " debstrnr
                delimited size into printregel.
                write printrecord from printregel
                after advancing 1 lines end-write.
                add 1 to pregeltel.
                move spaces to printregel.
                string
                spatie40 spatie20

                debpostkode "  " debwnplts
                delimited size into printregel.
                write printrecord from printregel end-write.
                add 1 to pregeltel.
                move spaces to printregel.
                close debstamfile.

        FAKTUURHOOFD-00.
                move spaces to printregel.
                string
                "                   "
                delimited size into printregel.
                write printrecord from printregel end-write.
                add 1 to pregeltel.
                move spaces to printregel.
                string
                "                   "
                delimited size into printregel.
                write printrecord from printregel end-write.
                add 1 to pregeltel.
                move spaces to printregel.
                string " " delimited size into printregel.
                write printrecord from printregel after advancing
                3 lines.
                add 3 to pregeltel.

      *    == is regel 25 ==
                move spaces to printregel.
                string datum(1:2) delimited size into jaar.
                string datum(3:2) delimited size into maand.
                string datum(5:2) delimited size into dag.
                string
                "    Faktuurdatum: " dag "-" maand "-" jaar
                delimited size
                into printregel.

                write printrecord from printregel after advancing 0
                lines.
                move spaces to printregel.
                string
                "    Paginanummer: "
                pagteller delimited size into printregel.
                write printrecord from printregel.
                move spaces to printregel.
                string
                "    ----------------------------------------------"
                "--------------------------------------"

                delimited size into printregel.
                write printrecord from printregel.

                add 2 to pregeltel.
                move spaces to printregel.
                string
                "    Datum/Regel            Kgs     Code      "
                "Omschrijving           Prijs         Totaal"
                delimited size into printregel.
                write printrecord from printregel.
                add 1 to pregeltel.
                move spaces to printregel.
                string
                "    ----------------------------------------------"
                "--------------------------------------"

                delimited size into printregel.
                write printrecord from printregel.
                add 1 to pregeltel.
      *    == op regel 28 afgedrukt==

                move spaces to printregel.
      *          move zero to totbedrexcl.

        FAKTUURTOTAAL.
                move spaces to printregel.
                string
      *          spatie40 aanpassing hp1010
                spatie40 spatie38

                "=========="
                delimited size into printregel.
                write printrecord from printregel after advancing
                2 lines end-write.
                add 2 to pregeltel.
                move spaces to printregel.
                move totbedrexcl to totbedrexclpr.
                compute bedragbtw rounded = totbedrexcl * 0.190.
                move bedragbtw to tekstbedragbtw.


                subtract pregeltel from 70 giving pregelcontrol.
                if pagteller > 001 subtract 011 from pregelcontrol
                giving pregelcontrol end-if.

                write printrecord from printregel after advancing
                pregelcontrol lines.
                move spaces to printregel.

                string
      *          spatie40 aanpassing hp1010
                spatie40 spatie38 totbedrexclpr
                delimited size into printregel end-string.
                write printrecord from printregel
                after advancing 2 lines end-write.
                move spaces to printregel.
                string
      *          spatie40 aanpassing hp1010
                spatie20 spatie10
                "Totaal excl b.t.w."
                delimited size into printregel.
                write printrecord from printregel after advancing
                0 lines
                end-write.

      *    == regel 42 ==
                move spaces to printregel.
                string
      *          spatie40 aanpassing hp1010
                spatie40 spatie38 tekstbedragbtw
                delimited size into printregel.
                write printrecord from printregel end-write.
                move spaces to printregel.
                string
      *          spatie40 aanpassing hp1010
                spatie20 spatie10
                "B.T.W. 19%"
                delimited size into printregel end-string.
                write printrecord from printregel after
                advancing 0 lines.
                move spaces to printregel.
                add totbedrexcl to bedragbtw giving bedragincl.
                move bedragincl to tekstbedragincl.
                string
      *          spatie40 aanpassing hp1010
                spatie40 spatie39
      *          spatie5 aanpassing hp1010

                "----------"
                delimited size into printregel end-string.
                write printrecord from printregel end-write.


                move spaces to printregel.
                string
      *          spatie40 aanpassing hp1010
                spatie40 spatie38 tekstbedragincl
                delimited size into printregel end-string.
                write printrecord from printregel after advancing
                2 lines end-write.
                move spaces to printregel.
                string
      *          spatie40 aanpassing hp1010
                spatie20 spatie10
                "Totaal te betalen in Euro's"
                delimited size into printregel end-string.
                write printrecord from printregel after advancing 0
                lines end-write.
                move spaces to printregel.
                string
      *          spatie40 aanpassing hp1010
                spatie40 spatie39
                "=========="
                delimited size into printregel.
                write printrecord from printregel end-write.

                move spaces to printregel.

                string
                "            "

                delimited size into
                printregel.
                write printrecord from printregel end-write.
                move spaces to printrecord.
                string

                 "    Onder vermelding van:"
                delimited size into printregel.
                write printrecord from printregel end-write.
                move spaces to printregel.

                move faktuurnr to tekstfaktuurnr.
                string
                "    Debiteurennummer: "
            debcode

                "    Faktuurnummer:  " tekstfaktuurnr
                delimited size into printregel.
                write printrecord from printregel end-write.


      *    == regel 44 ==
                move spaces to printregel.
                string

                "    ========"
                delimited size into printregel.

                write printrecord from printregel
                after advancing 2 lines end-write.
                move spaces to printregel.

                move zero to bedragbtw bedragincl.
                move zero to totbedrexcl.
        FAKTUURVOET.

                move spaces to printrecord.
                string
                "  " delimited size into printregel.
                write printrecord from printregel end-write.

       STOPPROGRAM.
      *     display "EOP" line 25 position 1.
           add 1 to kopietel.

      * aantal kopie fakturen (2 stuks)

           if kopietel < 2 move "J" to eerste
                            move 1 to pagteller
                            move 1 to detailregeltel
                            move 1 to pregeltel
                            go to lezen end-if.
           open output kfktfile.
           add 1 to faktuurnr.
           move faktuurnr to kfktnr.
           write kfktrecord end-write.
           close kfktfile.

       STOPPROGRAM-0.
           call program "bijlkies.cob" end-call.
           STOP RUN.



