       IDENTIFICATION DIVISION
       PROGRAM-ID. KITAP.
	   AUTHOR. MEHMET.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KITAP ASSIGN TO DISK ORGANIZATION LINE SEQUENTIAL.
           SELECT YAZICI ASSIGN TO PRINTER.
       DATA DIVISION.
       FILE SECTION.
       FD KITAP LABEL RECORD STANDARD VALUE OF FILE-ID "KITAP.DOS".
       01 KITAP-KAYDI.
           05 K-ADI PIC X(20).
           05 Y-ADI PIC X(20).
           05 KONUSU PIC X(10).
           05 B-YILI PIC 9(4).
       FD YAZICI LABEL RECORD OMITTED.
       01 YAZICI-KAYDI PIC X(80).
       01 CIKTI-SATIRI PIC X(80).
       WORKING-STORAGE SECTION.
       77 SIFRE PIC X(10) VALUE SPACE.
       77 CEVAP PIC X VALUE SPACE.
       77 KONTROL PIC 9 VALUE 0.
       77 SEC PIC 9 VALUE 0.
       77 KIT PIC X(20) VALUE SPACE.
       77 YAZ PIC X(20) VALUE SPACE.
       77 CIZGI PIC X(80) VALUE ALL "-".
       77 N PIC 9 VALUE 0.
       01 BASLIK.
           02 F PIC X(29) VALUE SPACE.
           02 F PIC X(22) VALUE "KITAP BILGILERI DOKUMU".
       01 BASLIK-1.
           05 F PIC X(8) VALUE SPACE.
           05 F PIC X(5) VALUE "SIRA".
           05 F PIC X(21) VALUE "KITAP ADI".
           05 F PIC X(21) VALUE "YAZAR ADI".
           05 F PIC X(11) VALUE "KONUSU".
           05 F PIC X(6) VALUE "B-YILI".
       01 DETAY.
           02 F PIC X(9) VALUE SPACES.
           02 S-NO PIC 99.
           02 F PIC XX VALUE SPACES.
           02 ADI PIC X(20).
           02 F PIC X VALUE SPACE.
           02 YADI PIC X(20).
           02 F PIC X VALUE SPACE.
           02 KONU PIC X(10).
           02 F PIC X VALUE SPACE.
           02 TARIH PIC X(4).
       SCREEN SECTION.
       01 EKRAN-1.
             05 LINE 1 COLUMN 1 BLANK SCREEN.
             05 LINE 10 COLUMN 10 PIC X(26) FROM ALL "�". 
             05 LINE 11 COLUMN 10 VALUE "SIFREYI GIRINIZ:".
             05 LINE 11 COLUMN 26 PIC X(10) SECURE USING SIFRE.
             05 LINE 12 COLUMN 10 PIC X(26) FROM ALL "�".
       01 EKRAN-2.
           02 LINE 1 COLUMN 1 BLANK SCREEN.
           02 LINE 1 COLUMN 1 BLINK VALUE "G.O.S.L.K.".
           02 LINE 2 COLUMN 1 PIC X(10) FROM ALL "�".
		   02 LINE 4 COLUMN 10 REVERSE-VIDEO VALUE
		      " M E H M E T * H A R A S ".
           02 LINE 7 COLUMN 10 PIC X(8) FROM ALL "�".     
           02 LINE 8 COLUMN 10 BLINK VALUE "ANA MENU".
           02 LINE 9 COLUMN 10 PIC X(28) FROM ALL "�".
           02 LINE 10 COLUMN 10 VALUE "[1]:DOSYA YARATMA".
           02 LINE 11 COLUMN 10 VALUE "[2]:BILGI GIRISI".
           02 LINE 12 COLUMN 10 VALUE "[3]:BILGILERDE DEGISIKLIK".
           02 LINE 13 COLUMN 10 VALUE "[4]:YAZAR ADINA GORE DOKUM".
           02 LINE 14 COLUMN 10 VALUE "[5]:YAZICI DOKUMU".
           02 LINE 15 COLUMN 10 VALUE "[6]:PROGRAMDAN CIKIS".
           02 LINE 16 COLUMN 10 PIC X(28) FROM ALL "�".
           02 LINE 19 COLUMN 10 BLINK VALUE "SECIMINIZ...:".
           02 LINE 19 COLUMN 23 PIC 9 BLINK USING SEC.
       01 EKRAN-3.
           02 LINE 1 COLUMN 1 BLANK SCREEN.
           02 LINE 1 COLUMN 1 BLINK VALUE " G.O.S.L.K.".
           02 LINE 2 COLUMN 1 PIC X(10) FROM ALL "�".
           02 LINE 6 COLUMN 10 PIC X(35) FROM ALL "�".
           02 LINE 7 COLUMN 10 VALUE "KITABIN ADI..:".
           02 LINE 8 COLUMN 10 VALUE "YAZARIN ADI..:".
           02 LINE 9 COLUMN 10 VALUE "KITABIN KONUSU...:".
           02 LINE 10 COLUMN 10 VALUE "BASIM YILI......:".
           02 LINE 11 COLUMN 10 PIC X(35) FROM ALL "�".
           02 LINE 7 COLUMN 25 PIC X(20) UNDERLINE USING K-ADI.
           02 LINE 8 COLUMN 25 PIC X(20) UNDERLINE USING Y-ADI.
           02 LINE 9 COLUMN 25 PIC X(10) UNDERLINE USING KONUSU.
           02 LINE 10 COLUMN 25 PIC X(4) UNDERLINE USING B-YILI.
       PROCEDURE DIVISION.
       BASLA. DISPLAY (1 1) ERASE. DISPLAY EKRAN-1. ACCEPT EKRAN-1.
           IF SIFRE NOT = "G.O.S.L.K." DISPLAY(1 1) ERASE. 
       AA. DISPLAY EKRAN-2.
       BB. ACCEPT EKRAN-2.
           IF SEC = 1 PERFORM YARAT
           ELSE IF SEC = 2 PERFORM GIRIS
           ELSE IF SEC = 3 PERFORM DEGISIKLIK
           ELSE IF SEC = 4 PERFORM E-DOKUMU
           ELSE IF SEC = 5 PERFORM Y-DOKUMU
           ELSE IF SEC = 6 GO SON ELSE
           DISPLAY (23 5) SEC (23 7) ". NOLU SECENEK YANLIS" GO BB.
           DISPLAY EKRAN-2. DISPLAY (23 5) SEC (23 7) 
           "NOLU ISLEM TAMAMLANMISTIR. ". GO BB.
       YARAT SECTION.
       YY. OPEN OUTPUT KITAP. CLOSE KITAP.
       GIRIS SECTION.
       GG1. OPEN EXTEND KITAP.
       GG2. DISPLAY EKRAN-3. MOVE ALL "." TO K-ADI Y-ADI KONUSU.
           MOVE 0 TO B-YILI.
           DISPLAY(5 10) "KITAP BILGILERI GIRISI".
       GG3. ACCEPT EKRAN-3. DISPLAY(23 1) ERASE (23 5)
           "VERILER DOGRUMU (E/H)". ACCEPT CEVAP. 
           IF CEVAP = " E "  WRITE KITAP-KAYDI. 
           IF CEVAP = " H " GO GG3. DISPLAY(23 1) ERASE (23 5)
           "GIRISE DEVAM EDIYORMUSUNUZ (E/H) ".
           ACCEPT CEVAP.
           IF CEVAP = " E " GO GG2.
           CLOSE KITAP.
       DEGISIKLIK SECTION.
       DD1. OPEN I-O KITAP.
           DISPLAY(1 1) ERASE.
           DISPLAY(5 5) "DEGISIKLIK YAPILACAK KAYDIN ISMI:".
           MOVE ALL "." TO KIT. ACCEPT(5 38)KIT.
       DD2. READ KITAP AT END GO UYARI.
           IF KIT NOT = K-ADI GO DD2.
           DISPLAY EKRAN-3. DISPLAY(23 1)ERASE (23 5) 
           "DEGISTIRMEK ISTEDIGINIZ KITAP BUMU:"
           ACCEPT CEVAP. IF CEVAP = " H " GO DD2.
       DD3. ACCEPT EKRAN-3. DISPLAY(23 1) ERASE.
           DISPLAY(23 5) "DOGRUMU(E/H):". ACCEPT CEVAP.
           IF CEVAP = " H " GO DD3.
           REWRITE KITAP-KAYDI. CLOSE KITAP.
           DISPLAY(23 5) "DEVAMMI(E/H)".
           ACCEPT CEVAP. IF CEVAP = " E " GO DD1 ELSE GO D-SON.
       UYARI. DISPLAY(23 1) ERASE (23 5)
               "ARADIGINIZ KITAP DOSYADA YOK ENT BAS". 
           ACCEPT CEVAP.
       D-SON. CLOSE KITAP.
       E-DOKUMU SECTION.
       MM1. OPEN INPUT KITAP.
       MM2. DISPLAY(1 1) ERASE. MOVE ALL "." TO YAZ.
           DISPLAY(5 5) "KITAPLARINI GORMEK ISTEDIGINIZ YAZAR:".
            ACCEPT(5 43) YAZ. 
   	   MM3.READ KITAP AT END GO YOK.
		   IF YAZ NOT = Y-ADI GO MM3. MOVE 0 TO KONTROL.
       M. DISPLAY(1 1) ERASE. DISPLAY(1 1) BASLIK. 
           DISPLAY(3 1)BASLIK-1. DISPLAY(4 1) CIZGI. 
           MOVE 2 TO LIN. 
       MNH. MOVE Y-ADI TO YADI. MOVE KONUSU TO KONU.
           MOVE B-YILI TO TARIH. MOVE K-ADI TO ADI.
		   ADD 1 TO KONTROL. MOVE KONTROL TO S-NO.
		   ADD 1 TO LIN. DISPLAY (LIN + 1 1) DETAY.
       MEH. READ KITAP AT END GO M-SON. IF YAZ NOT = Y-ADI GO MEH.
           IF LIN > 14 DISPLAY (23 20) "DIGER SAYFA ICIN ENT BAS".
           ACCEPT CEVAP GO MNH.		   
       YOK. DISPLAY (23 10) "BU YAZARA AIT HIC BILGI YOK ENT BAS".
           ACCEPT CEVAP.
       M-SON. DISPLAY(23 1) "DOKUM BITTI ENT BAS".
           ACCEPT CEVAP. CLOSE KITAP.
       Y-DOKUMU SECTION.
       YY1. OPEN INPUT KITAP. OPEN OUTPUT YAZICI.
           DISPLAY(1 1) ERASE.
       YY2. READ KITAP AT END GO UYAR.
       YY. MOVE 0 TO N.
           WRITE CIKTI-SATIRI FROM BASLIK AFTER 2.
           WRITE CIKTI-SATIRI FROM BASLIK-1. 
           WRITE CIKTI-SATIRI FROM CIZGI.
           MOVE 0 TO KONTROL.
       YY3. ADD 1 TO KONTROL. ADD 1 TO N. MOVE N TO S-NO.
           MOVE Y-ADI TO YADI. MOVE K-ADI TO ADI.
           MOVE KONUSU TO KONU. MOVE B-YILI TO TARIH.
           WRITE CIKTI-SATIRI FROM DETAY AFTER 2.
           IF KONTROL > 39 PERFORM YY.
           READ KITAP AT END GO Y-SON.
       UYAR. DISPLAY(23 5) "DOSYADA HIC KAYIT YOK ENT BAS:".
           ACCEPT CEVAP.
       Y-SON. CLOSE KITAP. CLOSE YAZICI.
       SON SECTION.
       SSS. DISPLAY(1 1) ERASE. STOP RUN.

                      

                                    
    