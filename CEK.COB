       IDENTIFICATION DIVISION.
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
