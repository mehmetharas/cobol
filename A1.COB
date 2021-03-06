       IDENTIFICATION DIVISION.
       PROGRAM-ID. BM4.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MIZAN ASSIGN TO DISK ORGANIZATION INDEXED ACCESS
           DYNAMIC RECORD KEY MUSTERI-KODU.
           SELECT CARI ASSIGN DISK ORGANIZATION INDEXED
           ACCESS DYNAMIC RECORD KEY CARI-ANAHTARI.
		   SELECT YAZICI ASSIGN TO PRINTER.
       DATA DIVISION.
       FILE SECTION.
	   FD YAZICI LABEL RECORD OMITTED.
	   01 SATIR PIC X(80).
       COPY B:FD.BLM.
       WORKING-STORAGE SECTION.
       COPY B:TANIM.BLM.
       SCREEN SECTION.
       COPY B:EKRAN.BLM.
       PROCEDURE DIVISION.
       BASLA. DISPLAY EKRAN1. DISPLAY (6 37) "Y A Z I C I D A".
       A10. ACCEPT EKRAN1. 
            IF N = 1 PERFORM CARI-DOKUM
            ELSE IF N = 2 PERFORM MIZAN-DOKUM
            ELSE IF N = 3 GO SONN
            ELSE DISPLAY (22 10) N DISPLAY (22 10)
            "NOLU SECENEK YANLIS LUTFEN DOGRUSUNU GIRINIZ" GO A10.
            DISPLAY EKRAN1. DISPLAY (6 37) "E K R A N D A".
            DISPLAY (22 10) N. DISPLAY (22 12)
            "NOLU ISLEM TAMAMLANMISTIR". GO A10.
       MIZAN-DOKUM SECTION.
       C10. PERFORM MIZ-BAS THRU MIZ-BAS-SON.
            PERFORM MIZAN-ISLEM UNTIL DD = "E". GO MIZAN-SON.
       MIZ-BAS. OPEN I-O MIZAN. DISPLAY (1 1) ERASE. MOVE "H" TO DD.
           DISPLAY (8 1) "ILK MUSTERI KODU...:".
           DISPLAY (10 1) "SON MUSTERI KODU...:".
           DISPLAY (21 1) "** < ILK MUSTERI KODU = SON > ".
           DISPLAY (21 31) "ANA MENUYE DONUS".
           DISPLAY (23 1) "** ILK MUSTERI KODU = 000 SON MUSTERI".
           DISPLAY (23 41) "KODU=ZZZ GIRINCE TUM DOKUMU ALIRSINIZ**".
           MOVE SPACES TO MUSTERI-KODU SON-MUSTERI-KODU.
       C20. ACCEPT (8 21) MUSTERI-KODU WITH PROMPT UPDATE.
           IF MUSTERI-KODU = "SON" MOVE "E" TO DD GO MIZ-BAS-SON.
           ACCEPT (10 21) SON-MUSTERI-KODU WITH PROMPT UPDATE.
           DISPLAY (15 5) "** DOGRUMU ?: **".
           ACCEPT (15 19) CEVAP WITH PROMPT.
           IF CEVAP = "H" GO C20. MOVE "H" TO CEVAP.
           DISPLAY (1 1) ERASE.
           START MIZAN KEY NOT LESS THAN MUSTERI-KODU INVALID KEY
           DISPLAY (24 1) "** START YAPAMIYORUM <ENT> BAS: **"
           ACCEPT CEVAP MOVE "E" TO DD GO MIZ-BAS-SON.
           READ MIZAN NEXT RECORD AT END
           MOVE "E" TO DD GO MIZ-BAS-SON.
           IF MUSTERI-KODU > SON-MUSTERI-KODU DISPLAY (24 1) 
           "** MUSTERI KODU SON KODUNU GECTI <ENT> BAS:  **"
           ACCEPT CEVAP MOVE "E" TO DD GO MIZ-BAS-SON.
           MOVE ALL ZEROS TO SAYFA-9 SIRA-9 G-BORC-M9 G-ALACAK-M9
           G-BORC-BAKIYE-M9 G-ALACAK-BAKIYE-M9. PERFORM MIZAN-BASLIK.
       MIZ-BAS-SON. EXIT.
       MIZAN-BASLIK. DISPLAY (1 1) ERASE. ADD 1 TO SAYFA-9.
           MOVE ALL ZEROS TO KONTROL S-BORC-M9 S-ALACAK-M9 
           S-BORC-BAKIYE-M9 S-ALACAK-BAKIYE-M9. MOVE 3 TO LIN.
           MOVE SAYFA-9 TO SAYFA-NO-B10. 
		   WRITE SATIR FROM BAS-7 AFTER 5.
           WRITE SATIR FROM BAS-12.       IDENTIFICATION DIVISION.
       PROGRAM-ID. CEKN.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 K PIC 9 VALUE 0.
       77 SIFRE PIC  X(10) VALUE SPACES.
       SCREEN SECTION.
       01 EKRAN2. 
	       05 BLANK SCREEN.
           05 LINE 1 COLUMN 1 REVERSE-VIDEO VALUE "BEBIM A.S.".
           05 LINE 2 COLUMN 1 REVERSE-VIDEO VALUE "* ANA MENU *" BLINK.
           05 LINE 3 COLUMN 1 PIC X(13) FROM ALL "*".
           05 LINE 5 COLUMN 5 VALUE "* CEK SENET TAKIBI PRG *".
           05 LINE 6 COLUMN 5 PIC X(51) FROM ALL "*".
           05 LINE 9 COLUMN 5 VALUE
            "*                                                 *".
           05 LINE 8 COLUMN 5 VALUE
            "* (1): VERI GIRISI / SILME / ODEMELERIN ISLENMESI *".
           05 LINE 7 COLUMN 5 VALUE 
            "*                                                 *".
           05 LINE 10 COLUMN 5 VALUE
            "* (2): LISTE DOKUMU ## EKRANDAN                   *".
           05 LINE 11 COLUMN 5 VALUE 
            "*                                                 *".
           05 LINE 12 COLUMN 5 VALUE
            "* (3): LISTE DOKUMU ## YAZICIDAN                  *".
           05 LINE 13 COLUMN 5 VALUE
            "*                                                 *".
           05 LINE 14 COLUMN 5 VALUE 
            "* (4): PROGRAMDAN CIKIS                           *".
           05 LINE 15 COLUMN 5 VALUE
            "*                                                 *".
           05 LINE 16 COLUMN 5 PIC X(51) FROM ALL "*".
           05 LINE 17 COLUMN 5 VALUE 
            "*                                                 *".
           05 LINE 18 COLUMN 5 VALUE 
            "* HANGISINI ISTERSINIZ                            *".
           05 LINE 18 COLUMN 30 PIC 9 USING K.
           05 LINE 19 COLUMN 5 PIC X(51) FROM ALL "*".
       PROCEDURE DIVISION.
       OYNA. DISPLAY (1 1) ERASE. ACCEPT (10 5) SIFRE
           WITH PROMPT NO-ECHO. IF SIFRE NOT = "BEBIM A.S."
           DISPLAY (1 1) ERASE STOP RUN. DISPLAY EKRAN2.
       AA0. ACCEPT EKRAN2. IF K = 1 CALL "CSEN1" ELSE IF K = 2 CALL
           "CSEN2" ELSE IF K = 3 CALL "CSEN3" ELSE IF K = 4 DISPLAY
           (1 1) ERASE STOP RUN ELSE DISPLAY (21 5) K DISPLAY (21 8)
           "NOLU SECENEK YANLIS LUTFEN DOGRUSUNU GIRINIZ"  GO AA0.
           DISPLAY (1 1) ERASE. DISPLAY EKRAN2. 
           DISPLAY (21 5) K DISPLAY (21 8)
           "NOLU ISLEM TAMAMLANMISTIR". GO AA0.
       END-OF-JOB. 
  (          �!       A1      COB           �!  �&  BIROL   COB           !  �   AYKUT   OBJ           /! 
�(  CARI    DOS           �! (    ODEV    COB           `! 1 
  ODEV    OBJ           p! | t  ODEV    EXE           �! � ��  CARI    KEY           �! &    A       OBJ           �! � �  BIROL   OBJ           ]! "  A       COB           �!  "  AYKUT   COB           ,!  a%  C1      BAT            ! y    C       BAT           5! z    TANIM   BLM           �! �   