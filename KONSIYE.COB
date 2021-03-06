       IDENTIFICATION DIVISION.
       PROGRAM-ID. KONSIYE.
       AUTHOR. MEHMET HARAS.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KON1 ASSIGN TO DISK ORGANIZATION INDEXED
           ACCESS DYNAMIC RECORD KEY KOD1.
           SELECT KON2 ASSIGN TO DISK ORGANIZATION INDEXED
           ACCESS DYNAMIC RECORD KEY KOD2.
           SELECT YAZICI ASSIGN TO PRINTER.
       DATA DIVISION.
       FILE SECTION.
       FD KON1 LABEL RECORD STANDARD VALUE OF FILE-ID "KON1.DOS".
       01 KON1-KAYDI.
           02 KOD1 PIC X(15).
           02 UNVAN1 PIC X(37).
           02 ADRES PIC X(22).
       FD KON2 LABEL RECORD STANDARD VALUE OF FILE-ID "KON2.DOS".
       01 KON2-KAYDI.
           02 KOD2.
              05 KOD11 PIC X(15).
              05 I-NO PIC 9(8).
              05 TARIH.
                   10 I-GUNU PIC 9(2).
                   10 I-AYI PIC 9(2).
                   10 I-YILI PIC 9(4).
           02 MAL-ISMI PIC X(29).
           02 TUTAR PIC 9(11).
       FD YAZICI LABEL RECORD OMITTED.
       01 YAZ PIC X(80).
       WORKING-STORAGE SECTION.
       77 C-KOD PIC X(15) VALUE SPACES.
       77 SIRA PIC 9(3) VALUE 0.
       77 KSIRA PIC 9(3) VALUE 0.
       77 TUT-S PIC 9(11) VALUE 0.
       77 TUT-G PIC 9(11) VALUE 0.
       77 BOSLUK PIC X(80) VALUE SPACES.
       77 ILK-KOD PIC X(15) VALUE SPACES.
       77 SON-KOD PIC X(15) VALUE SPACES.
       77 S PIC 99 VALUE 0.
       77 C PIC X VALUE SPACE.
       77 GUN PIC 9(2) VALUE 0.
       77 AY PIC 9(2) VALUE 0.
       77 YIL PIC 9(4) VALUE 0.
       01 B1.    
           02 F PIC X(80) VALUE 
              "Y � K S E L   T A R I M   A L E T L E R �".
           02 F PIC X(80) VALUE "SAN.VE T�C. LTD. �T�.".
       01 BAS1.
           02 F PIC X(7) VALUE "KODU:".
           02 M-KOD PIC X(15).
           02 F PIC X(7) VALUE SPACES.
           02 F PIC X(30) VALUE "KONSIYE MAL DOKUMU".
       01 BAS2.
           02 F PIC X(9) VALUE "UNVANI:".
           02 M-UNVAN PIC X(37).
           02 F PIC XX VALUE SPACES.
           02 M-ADRES PIC X(22).
       01 BAS3.
           02 F PIC X(31) VALUE
              "SR     I-TARIHI     I-NOSU     ".
           02 F PIC X(43) VALUE
              "GIDEN-MAL-ISMI                   TUTARI". 
       01 BAS4.
           02 SR PIC ZZ9.
           02 F PIC XXX VALUE SPACES.
           02 Y-GUN PIC 9(2).
           02 F PIC X VALUE "/".
           02 Y-AY PIC 9(2).
           02 F PIC X VALUE "/".
           02 Y-YIL PIC 9(4).
           02 F PIC XXX VALUE SPACES.
           02 Y-NO PIC ZZZZZZZ9.
           02 F PIC XXX VALUE SPACES.
           02 Y-MAL PIC X(29).
           02 F PIC XXX VALUE SPACES.
           02 TUT PIC ZZ,ZZZ,ZZZ,ZZ9.
       01 BAS5.
           02 F PIC X(80) VALUE ALL "-".
       01 BAS6.
           02 F PIC X(15) VALUE "SAYFA TOPLAMI:".
           02 SY-TOP PIC ZZ,ZZZ,ZZZ,ZZ9.
       01 BAS7.
           02 F PIC X(15) VALUE "GENEL  TOPLAM:".
           02 GY-TOP PIC ZZ,ZZZ,ZZZ,ZZ9.
       01 BAS8.
           02 F PIC X(49) VALUE 
              "SR   KODU           UNVANI".
           02 F PIC X(22) VALUE "            ADRES�".
       01 BAS9.
           02 SIRA1 PIC ZZ9.
           02 F PIC X VALUE SPACE.
           02 Y-KODD PIC X(15).
		   02 F PIC X VALUE SPACE.
           02 Y-UNVANN PIC X(37).
           02 F PIC X VALUE SPACE.
           02 ADRESS PIC X(22).
       SCREEN SECTION.
       01 E1.
           02 BLANK SCREEN.
           02 LINE 1 COLUMN 1 REVERSE-VIDEO VALUE
              "--- Y�KSEL TARIM ALETLER� ---".
           02 LINE 2 COLUMN 1 REVERSE-VIDEO VALUE
              "--- SAN.VE T�V. LTD. �T�. ---".
           02 LINE 3 COLUMN 27 REVERSE-VIDEO VALUE
              "--- KONSIYE PROGRAMI ---".
           02 LINE 5 COLUMN 7 VALUE "1:KOD G�R���".
           02 LINE 6 COLUMN 7 VALUE "2:KOD �PTAL".
           02 LINE 7 COLUMN 7 VALUE "3:KOD DE����KL���".
           02 LINE 8 COLUMN 7 VALUE "4:KONS�YE G�R���".
           02 LINE 9 COLUMN 7 VALUE "5:KONS�YE �PTAL".
           02 LINE 10 COLUMN 7 VALUE "6:KONS�YE DE����KL���".
           02 LINE 11 COLUMN 7 VALUE "7:KOD D�K�M� EKRAN".
           02 LINE 12 COLUMN 7 VALUE "8:KOD D�K�M� YAZICI".
           02 LINE 13 COLUMN 7 VALUE "9:KONS�YE D�K�M� EKRAN".
           02 LINE 14 COLUMN 7 VALUE "10:KONS�YE D�K�M� YAZICI".
           02 LINE 15 COLUMN 7 VALUE "11:�IKI�".
           02 LINE 17 COLUMN 7 REVERSE-VIDEO VALUE "SE��M�N�Z ==>".
           02 LINE 17 COLUMN 21 PIC 99 USING S.
       01 E2.
           02 BLANK SCREEN.
           02 LINE 1 COLUMN 1 REVERSE-VIDEO
              "--- Y�KSEL TARIM ALETLER� ---".
           02 LINE 2 COLUMN 1 REVERSE-VIDEO VALUE
              "--- SAN.VE T�C. LTD. �T�. ---".
           02 LINE 4 COLUMN 7 VALUE "M��TER�N�N".
           02 LINE 5 COLUMN 7 VALUE "KODUNU G�R�N�Z  :".
           02 LINE 5 COLUMN 25 PIC X(15) USING KOD1.
       01 EE2. 
           02 LINE 6 COLUMN 7 VALUE "UNVANINI G�R�N�Z:".
           02 LINE 7 COLUMN 7 VALUE "ADRES�N� G�R�N�Z:".
           02 LINE 6 COLUMN 25 PIC X(37) USING UNVAN1.
           02 LINE 7 COLUMN 25 PIC X(22) USING ADRES.
       01 E3.
           02 BLANK SCREEN.
           02 LINE 1 COLUMN 1 REVERSE-VIDEO
              "--- Y�KSEL TARIM ALETLER� ---".
           02 LINE 2 COLUMN 1 REVERSE-VIDEO VALUE
              "--- SAN.VE T�C. LTD. �T�. ---".
           02 LINE 5 COLUMN 7 VALUE "KODUNU G�R�N�Z  :".
           02 LINE 6 COLUMN 7 VALUE "�RSAL�YE TAR�H� :".
           02 LINE 6 COLUMN 29 VALUE " /".
           02 LINE 6 COLUMN 33 VALUE " /".
           02 LINE 7 COLUMN 7 VALUE "�RSAL�YE NOSU   :".
           02 LINE 5 COLUMN 27 PIC X(15) USING KOD11.           
           02 LINE 6 COLUMN 27 AUTO PIC 9(2) USING I-GUNU.
           02 LINE 6 COLUMN 31 AUTO PIC 9(2) USING I-AYI.
           02 LINE 6 COLUMN 35 AUTO PIC 9(4) USING I-YILI.
           02 LINE 7 COLUMN 27 PIC 9(8) USING I-NO.           
       01 E33.  02 LINE 8 COLUMN 7 VALUE "A�IKLAMA        :".
           02 LINE 9 COLUMN 7 VALUE "TUTARI          :".
           02 LINE 8 COLUMN 27 PIC X(29) USING MAL-ISMI.
           02 LINE 9 COLUMN 27 PIC 9(11) USING TUTAR.
       01 E4.
           02 BLANK SCREEN.
           02 LINE 1 COLUMN 1 REVERSE-VIDEO
              "--- Y�KSEL TARIM ALETLER� ---".
           02 LINE 2 COLUMN 1 REVERSE-VIDEO VALUE
              "--- SAN.VE T�C. LTD. �T�. ---".
           02 LINE 5 COLUMN 7 VALUE "M��TER� KODUNU G�R�N�Z:".
           02 LINE 5 COLUMN 37 PIC X(15) USING C-KOD.
       01 E99.
           02 BLANK SCREEN.
           02 LINE 11 COLUMN 11 VALUE "BURADA DOSYA TEKRAR YARATILIR".
           02 LINE 13 COLUMN 11 VALUE "VE B�T�N B�LG�LER S�L�N�R".
           02 LINE 15 COLUMN 11 VALUE 
              "B�LG�LER�N S�L�NMEMES� ���N ESC BAS".
           02 LINE 15 COLUMN 55 PIC X USING C.
       01 EKRAN1.
           02 BLANK SCREEN.
           02 LINE 1 COLUMN 1 REVERSE-VIDEO VALUE
              "--- Y�KSEL TARIM ALETLER� ---".
           02 LINE 2 COLUMN 1 REVERSE-VIDEO VALUE
              "--- SAN.VE T�C. LTD. �T�. ---".               
           02 LINE 11 COLUMN 11 VALUE "ILK KODU G�R�N�Z".
           02 LINE 13 COLUMN 11 VALUE "SON KODU G�R�N�Z".
           02 LINE 11 COLUMN 29 PIC X(15) USING ILK-KOD.
           02 LINE 13 COLUMN 29 PIC X(15) USING SON-KOD.
       PROCEDURE DIVISION.
       BASLA. DISPLAY (1 1) ERASE. 
           DISPLAY (11 27) "Y�KSEL TARIM ALETLER�".
           DISPLAY (13 27) "SAN.VE T�C. LTD. �T�.".
           DISPLAY (17 35) 
              "--- Haz�rlayan: M E H M E T --- H A R A S ---".
           DISPLAY (23 11) "ANA MENU ���N ENT BAS".
           ACCEPT C.
       KMA1. DISPLAY E1.
       KMA2. ACCEPT E1. IF S = 01 PERFORM KOD-GIR
           ELSE IF S = 02 PERFORM KOD-IPTAL
           ELSE IF S = 03 PERFORM KOD-DEG
           ELSE IF S = 04 PERFORM KON-GIR
           ELSE IF S = 05 PERFORM KON-IPTAL
           ELSE IF S = 06 PERFORM KON-DEG
           ELSE IF S = 07 PERFORM KOD-EK
           ELSE IF S = 08 PERFORM KOD-YAZ
           ELSE IF S = 09 PERFORM KON-EK
           ELSE IF S = 10 PERFORM KON-YAZ
           ELSE IF S = 11 GO K-P-SON
           ELSE IF S = 99 PERFORM DOS-YARAT
           ELSE DISPLAY (23 7) S DISPLAY (23 9) 
           "  .NOLU ��LEM YANLI� L�TFEN DO�RU G�R�N�Z" GO KMA2.
           GO KMA1.
       DOS-YARAT SECTION.
       DOS11. DISPLAY E99. ACCEPT E99 ON ESCAPE GO DOS-SON.
           DISPLAY (21 11) "EM�N M�S�N�Z?". ACCEPT (21 27) C.
           IF C = "H" GO DOS-SON. OPEN OUTPUT KON1. CLOSE KON1.
           DISPLAY (21 1) ERASE. 
           DISPLAY (21 11) "KOD DOSYASI YARATILDI KONS�YE DOSYASINI".
           DISPLAY (23 11) "YARATMAK �ST�YOR MUSUNUZ?".
           ACCEPT (23 45) C. IF C = "H" GO DOS-SON.
           OPEN OUTPUT KON2. CLOSE KON2.
           DISPLAY (1 1) ERASE. 
           DISPLAY (11 11) "DOSYALAR TEKRAR YARATILDI ENT BAS".
           ACCEPT C.
       DOS-SON. EXIT.
       KOD-GIR SECTION.
       KD1. OPEN I-O KON1.
       KD2. MOVE SPACES TO KOD1 UNVAN1 ADRES.
       KD3. DISPLAY E2. DISPLAY (3 7) "YEN� M��TER� KODU G�R���".
            DISPLAY (25 1) "ESC �LE �IKILIR".
       KD4. ACCEPT E2 ON ESCAPE GO KON-G-SON.
           READ KON1 INVALID KEY GO KD5. DISPLAY (24 1) ERASE.
           DISPLAY EE2. DISPLAY (24 11) "BU KOD ��LENM�� ENT BAS".
           ACCEPT C. GO KD2.
       KD5. DISPLAY EE2. 
           DISPLAY (23 1) "CTRL-B  (^B)  �LE B�R ALAN GER� G�D�L�R".
       KD6. ACCEPT EE2 ON ESCAPE GO KON-G-SON.            
           DISPLAY (21 11) "VER�LER DO�RUMU". ACCEPT (21 29) C.
           IF C = "H" GO KD6.
           WRITE KON1-KAYDI INVALID KEY DISPLAY (24 1) ERASE
           DISPLAY (24 1) "--- YAZAMIYORUM D�SK DOLU/BOZUK ENT BAS ---"
           ACCEPT C GO KON-G-SON. GO KD2.
       KON-G-SON. CLOSE KON1.
       KOD-IPTAL SECTION.
       KI1. OPEN I-O KON1 KON2.
       KI2. MOVE SPACES TO KOD1. MOVE 0 TO I-NO I-GUNU I-AYI I-YILI.
       KI3. DISPLAY E2. DISPLAY (3 7) "M��TER� KODU S�L�NMES�".
            DISPLAY (25 1) "ESC �LE �IKILIR".
       KI4. ACCEPT E2 ON ESCAPE GO KON-I-SON. MOVE KOD1 TO KOD11.
           READ KON1 INVALID KEY DISPLAY (24 1) ERASE
           DISPLAY (24 11) "BU KOD ��LENMEM�� ENT BAS" ACCEPT C GO KI2.
           DISPLAY EE2. 
           DISPLAY (21 11) "BU KAYDI S�LMEK �ST�YORMUSUNUZ". 
           ACCEPT (21 43) C. IF C = "H" GO KI2.
           START KON2 KEY NOT LESS KOD2 INVALID KEY DISPLAY (24 1) ERASE
           DISPLAY (24 1) 
           "-- KONS�YE DOSYASINA START YAPAMIYORUM ENT BAS ---"
           ACCEPT C GO KI5.
       K-I-AR. READ KON2 NEXT AT END GO KI5. 
           IF KOD11 NOT = KOD1 GO K-I-AR.
           DISPLAY (24 1) ERASE.
           DISPLAY (24 1) 
           "BU KODLU YERE   KONS�YE MAL G�TM�� S�LEMEZS�N�Z ENT BAS ".
           ACCEPT C. GO KI2.
       KI5. DELETE KON1 INVALID KEY DISPLAY (24 1) ERASE
           DISPLAY (24 1) "--- S�LEM�YORUM  D�SK BOZUK ENT BAS ---"
           ACCEPT C GO KON-I-SON. GO KI2.
       KON-I-SON. CLOSE KON1 KON2.
       KOD-DEG SECTION.
       KE1. OPEN I-O KON1.
       KE2. MOVE SPACES TO KOD1 UNVAN1 ADRES.
       KE3. DISPLAY E2. DISPLAY (3 7) "M��TER� KODUNDA DE����KL�K".
            DISPLAY (25 1) "ESC �LE �IKILIR".
       KE4. ACCEPT E2 ON ESCAPE GO KON-E-SON.
           READ KON1 INVALID KEY DISPLAY (24 1) ERASE
           DISPLAY (24 11) "BU KOD YOK ENT BAS" ACCEPT C  GO KE2.
       KE5. DISPLAY EE2. 
           DISPLAY (23 1) "CTRL-B  (^B)  �LE B�R ALAN GER� G�D�L�R".
       KE6. ACCEPT EE2 ON ESCAPE GO KON-E-SON.
           DISPLAY (21 11) "VER�LER DO�RUMU". ACCEPT (21 27) C.
           IF C = "H" GO KE6.
           REWRITE KON1-KAYDI INVALID KEY DISPLAY (24 1) ERASE
           DISPLAY (24 1) "--- YAZAMIYORUM D�SK DOLU/BOZUK ENT BAS ---"
           ACCEPT C GO KON-E-SON. GO KE2.
       KON-E-SON. CLOSE KON1.
       KON-GIR SECTION.     
       KSG1. OPEN I-O KON2 INPUT KON1.
       KSG2. MOVE SPACES TO KOD1 KOD11 MAL-ISMI.
           MOVE 0 TO I-GUNU I-AYI I-YILI TUTAR I-NO.
       KSG3. DISPLAY E3. DISPLAY (3 7) "KONS�YE G�R���".
            DISPLAY (25 1) "ESC �LE �IKILIR".
           DISPLAY (23 1) "CTRL-B  (^B)  �LE B�R ALAN GER� G�D�L�R".
       KSG4. ACCEPT E3 ON ESCAPE GO KON-SG-SON. MOVE KOD11 TO KOD1.
           READ KON1 INVALID KEY DISPLAY (24 1) ERASE
           DISPLAY (24 11) "BU KOD ��LENMEM�� ENT BAS"
           ACCEPT C GO KSG2.
           READ KON2 INVALID KEY GO AQYT1. DISPLAY (24 1) ERASE.
           DISPLAY (24 1) "B�YLE B�R KAYIT MEVCUT ENT BAS" 
           ACCEPT C GO KSG2.
       AQYT1. DISPLAY E33.
       KSGQ1. ACCEPT E33 ON ESCAPE GO KON-SG-SON.
           DISPLAY (21 11) "VER�LER DO�RUMU". ACCEPT (21 29) C.
           IF C = "H" GO KSGQ1.
           WRITE KON2-KAYDI INVALID KEY DISPLAY (24 1) ERASE
           DISPLAY (24 1) "--- YAZAMIYORUM D�SK DOLU/BOZUK ENT BAS ---"
           ACCEPT C GO KON-SG-SON. GO KSG2.
       KON-SG-SON. CLOSE KON1 KON2.
       KON-DEG SECTION.     
       KDG1. OPEN I-O KON2 INPUT KON1.
       KDG2. MOVE SPACES TO KOD1 KOD11 MAL-ISMI.
           MOVE 0 TO I-GUNU I-AYI I-YILI TUTAR I-NO.
       KDG3. DISPLAY E3. DISPLAY (3 7) "KONS�YE DE����KL���".
            DISPLAY (25 1) "ESC �LE �IKILIR".
           DISPLAY (23 1) "CTRL-B  (^B)  �LE B�R ALAN GER� G�D�L�R".
       KDG4. ACCEPT E3 ON ESCAPE GO KON-DG-SON. MOVE KOD11 TO KOD1.
           READ KON1 INVALID KEY DISPLAY (24 1) ERASE
           DISPLAY (24 11) "BU KOD ��LENMEM�� ENT BAS"
           ACCEPT C GO KDG2.
           READ KON2 INVALID KEY DISPLAY (24 1) ERASE DISPLAY (24 1)
           "B�YLE B�R KAYIT MEVCUT DE��L ENT BAS" ACCEPT C GO KDG2.
           DISPLAY E33.
       KDGQ1. ACCEPT E33 ON ESCAPE GO KON-DG-SON.
           DISPLAY (21 11) "VER�LER DO�RUMU". ACCEPT (21 29) C.
           IF C = "H" GO KDGQ1.
           REWRITE KON2-KAYDI INVALID KEY DISPLAY (24 1) ERASE
           DISPLAY (24 1) "--- YAZAMIYORUM D�SK DOLU/BOZUK ENT BAS ---"
           ACCEPT C GO KON-DG-SON. GO KDG2.
       KON-DG-SON. CLOSE KON1 KON2.
       KON-IPTAL SECTION.     
       KIG1. OPEN I-O KON2 INPUT KON1.
       KIG2. MOVE SPACES TO KOD1 KOD11 MAL-ISMI.
           MOVE 0 TO I-GUNU I-AYI I-YILI TUTAR I-NO.
       KIG3. DISPLAY E3. DISPLAY (3 7) "KONS�YE �PTAL�".
            DISPLAY (25 1) "ESC �LE �IKILIR".
           DISPLAY (23 1) "CTRL-B  (^B)  �LE B�R ALAN GER� G�D�L�R".
       KIG4. ACCEPT E3 ON ESCAPE GO KON-IG-SON. MOVE KOD11 TO KOD1.
           READ KON1 INVALID KEY DISPLAY (24 1) ERASE
           DISPLAY (24 11) "BU KAYIT YOK ENT BAS"
           ACCEPT C GO KIG2.
           READ KON2 INVALID KEY DISPLAY (24 1) ERASE DISPLAY (24 1)
           "B�YLE B�R KAYIT MEVCUT DE��L ENT BAS" ACCEPT C GO KIG2.
           DISPLAY E33.
           DISPLAY (21 11) "BU KAYDI S�LMEK �ST�YOR MUSUNUZ". 
           ACCEPT (21 47) C. IF C = "H" GO KIG2.
           DELETE KON2 INVALID KEY DISPLAY (24 1) ERASE
           DISPLAY (24 1) "--- S�LEM�YORUM DISK BOZUK ENT BAS ---"
           ACCEPT C GO KON-IG-SON. GO KIG2.
       KON-IG-SON. CLOSE KON1 KON2.
       KOD-EK SECTION.
       K-E-1. OPEN INPUT KON1.
       K-E-2. MOVE SPACES TO KOD1 ILK-KOD SON-KOD.
           MOVE 0 TO LIN SIRA.
       K-E-3. DISPLAY EKRAN1. DISPLAY (3 7) "EKRANA KOD D�K�M�".
            DISPLAY (25 1) "ESC �LE �IKILIR".
       K-E-4. ACCEPT EKRAN1 ON ESCAPE GO K-E-SONN.
           START KON1 KEY NOT LESS KOD1 INVALID KEY DISPLAY (24 1) ERASE
           DISPLAY (24 1) "DOSYAYA START YAPILAMIYOR ENT BAS" ACCEPT C
           GO K-E-2. DISPLAY (24 1) ERASE. 
           DISPLAY (24 1) "L�TFEN BEKLEY�N�Z".
           READ KON1 NEXT AT END GO K-E-2. GO K-E-6.
       K-E-BAS. DISPLAY (1 1) ERASE. DISPLAY (1 1) B1.
           DISPLAY (3 1) BAS8. DISPLAY (4 1) BAS5.
       K-E-5. READ KON1 NEXT AT END GO K-E-SONN.
       K-E-6. IF ILK-KOD > KOD1 GO K-E-5.
           IF SON-KOD < KOD1 GO K-E-5.
           ADD 1 TO LIN. ADD 1 TO SIRA. MOVE SIRA TO SIRA1.
           MOVE KOD1 TO Y-KODD. MOVE UNVAN1 TO Y-UNVANN.
           MOVE ADRES TO ADRESS. IF LIN = 1 PERFORM K-E-BAS.
           DISPLAY (LIN + 4 1) BAS9.
           IF LIN = 15 DISPLAY (LIN + 5 1) BAS5
           DISPLAY (24 1) ERASE DISPLAY (24 1) 
           "D�K�ME DEVAM ETMEK �ST�YORMUSUNUZ" ACCEPT (24 45) C
           IF C = "H" GO K-E-2 ELSE MOVE 0 TO LIN.
           GO K-E-5.
       K-E-SONN. IF LIN > 0 DISPLAY (LIN + 5 1) BAS5.
           DISPLAY (24 1) ERASE. DISPLAY (24 1) "ANA MENU ���N ENT BAS".
           ACCEPT C. CLOSE KON1.
       KOD-YAZ SECTION.
       K-Y-1. OPEN INPUT KON1 OUTPUT YAZICI.
       K-Y-2. MOVE SPACES TO KOD1 ILK-KOD SON-KOD.
           MOVE 0 TO KSIRA SIRA.
       K-Y-3. DISPLAY EKRAN1. DISPLAY (3 7) "YAZICIYA KOD D�K�M�".
            DISPLAY (25 1) "ESC �LE �IKILIR".
       K-Y-4. ACCEPT EKRAN1 ON ESCAPE GO K-Y-SONN.
           START KON1 KEY NOT LESS KOD1 INVALID KEY DISPLAY (24 1) ERASE
           DISPLAY (24 1) "DOSYAYA START YAPILAMIYOR ENT BAS" ACCEPT C
           GO K-Y-2. DISPLAY (24 1) ERASE. 
           DISPLAY (24 1) "L�TFEN BEKLEY�N�Z".
           READ KON1 NEXT AT END GO K-Y-2. GO K-Y-6.
       K-Y-BAS. WRITE YAZ FROM B1 AFTER 7.
           WRITE YAZ FROM BAS8. WRITE YAZ FROM BAS5.
       K-Y-5. READ KON1 NEXT AT END GO K-Y-SONN.
       K-Y-6. IF ILK-KOD > KOD1 GO K-Y-5.
           IF SON-KOD < KOD1 GO K-Y-5.
           ADD 1 TO KSIRA. ADD 1 TO SIRA. MOVE SIRA TO SIRA1.
           MOVE KOD1 TO Y-KODD. MOVE UNVAN1 TO Y-UNVANN.
           MOVE ADRES TO ADRESS. IF KSIRA = 1 PERFORM K-Y-BAS.
           WRITE YAZ FROM BAS9.
           IF KSIRA = 45 WRITE YAZ FROM BAS5 WRITE YAZ FROM BOSLUK
           DISPLAY (24 1) ERASE DISPLAY (24 1) 
           "D�K�ME DEVAM ETMEK �ST�YORMUSUNUZ" ACCEPT (24 45) C
           IF C = "H" GO K-Y-2 ELSE MOVE 0 TO KSIRA.
           GO K-Y-5.
       K-Y-SONN. IF KSIRA > 0 WRITE YAZ FROM BAS5 WRITE YAZ FROM BOSLUK.
           DISPLAY (24 1) ERASE. DISPLAY (24 1) "ANA MENU ���N ENT BAS".
           ACCEPT C. CLOSE KON1 YAZICI.
       KON-EK SECTION.
       K-NN-1. OPEN INPUT KON1 KON2.
       K-NN-2. MOVE SPACES TO C-KOD.
           MOVE 0 TO LIN SIRA I-GUNU I-AYI I-YILI I-NO.
       K-NN-3. DISPLAY E4. DISPLAY (3 7) "EKRANA KONSIYE D�K�M�".
            DISPLAY (25 1) "ESC �LE �IKILIR".
       K-NN-4. ACCEPT E4 ON ESCAPE GO K-NN-SONN. 
           MOVE C-KOD TO KOD11 KOD1.
           READ KON1 INVALID KEY DISPLAY (24 1) ERASE
           DISPLAY (24 11) "BU KOD ��LENMEM�� ENT BAS"
           ACCEPT C GO K-NN-2. MOVE 0 TO TUT-S TUT-G.
           START KON2 KEY NOT LESS KOD2 INVALID KEY DISPLAY (24 1) ERASE
           DISPLAY (24 1) "DOSYAYA START YAPILAMIYOR ENT BAS" ACCEPT C
           GO K-NN-2. DISPLAY (24 1) ERASE. 
           DISPLAY (24 1) "L�TFEN BEKLEY�N�Z".
           READ KON2 NEXT AT END GO K-NN-2. GO K-NN-6.
       K-NN-BAS. DISPLAY (1 1) ERASE. DISPLAY (1 1) B1.
           DISPLAY (3 1) BAS1. DISPLAY (4 1) BAS2. DISPLAY (5 1) BAS3.
           DISPLAY (6 1) BAS5.
       K-NN-5. READ KON2 NEXT AT END GO K-NN-SONN.
       K-NN-6. IF C-KOD NOT =  KOD11 GO K-NN-5.
           ADD 1 TO LIN. ADD 1 TO SIRA. MOVE SIRA TO SR.
           MOVE C-KOD TO M-KOD. MOVE UNVAN1 TO M-UNVAN.
           MOVE ADRES TO M-ADRES. MOVE I-GUNU TO Y-GUN.
           MOVE I-AYI TO Y-AY. MOVE I-YILI TO Y-YIL.
           MOVE I-NO TO Y-NO. MOVE MAL-ISMI TO Y-MAL.
           MOVE TUTAR TO TUT. ADD TUTAR TO TUT-S.
           ADD TUTAR TO TUT-G.
           IF LIN = 1 PERFORM K-NN-BAS.
           DISPLAY (LIN + 7 1) BAS4.
           IF LIN = 13 MOVE TUT-S TO SY-TOP MOVE TUT-G TO GY-TOP
           DISPLAY (LIN + 8 1) BAS5 DISPLAY (LIN + 9 1) BAS6
           DISPLAY (LIN + 10 1) BAS7 DISPLAY (24 1) ERASE DISPLAY (24 1)
           "D�K�ME DEVAM ETMEK �ST�YORMUSUNUZ" ACCEPT (24 45) C
           IF C = "H" GO K-NN-2 ELSE MOVE 0 TO LIN.
           GO K-NN-5.
       K-NN-SONN. IF LIN > 0 DISPLAY (LIN + 8 1) BAS5 
           MOVE TUT-S TO SY-TOP MOVE TUT-G TO GY-TOP
           DISPLAY (LIN + 9 1) BAS6 DISPLAY (LIN + 10 1) BAS7.
           DISPLAY (24 1) ERASE. DISPLAY (24 1) "ANA MENU ���N ENT BAS".
           ACCEPT C. CLOSE KON2 KON1.
       KON-YAZ SECTION.
       K-YN-1. OPEN INPUT KON1 KON2 OUTPUT YAZICI.
       K-YN-2. MOVE SPACES TO C-KOD.
            MOVE 0 TO KSIRA SIRA I-GUNU I-AYI I-YILI I-NO.
       K-YN-3. DISPLAY E4. DISPLAY (3 7) "YAZICIYA KONSIYE D�K�M�".
            DISPLAY (25 1) "ESC �LE �IKILIR".
       K-YN-4. ACCEPT E4 ON ESCAPE GO K-YN-SONN. 
           MOVE C-KOD TO KOD11 KOD1.
           READ KON1 INVALID KEY DISPLAY (24 1) ERASE
           DISPLAY (24 11) "BU KOD ��LENMEM�� ENT BAS"
           ACCEPT C GO K-YN-2. MOVE 0 TO TUT-S TUT-G.
           START KON2 KEY NOT LESS KOD2 INVALID KEY DISPLAY (24 1) ERASE
           DISPLAY (24 1) "DOSYAYA START YAPILAMIYOR ENT BAS" ACCEPT C
           GO K-YN-2. DISPLAY (24 1) ERASE. 
           DISPLAY (24 1) "L�TFEN BEKLEY�N�Z".
           READ KON2 NEXT AT END GO K-YN-2. GO K-YN-6.
       K-YN-BAS. WRITE YAZ FROM B1 AFTER 7. WRITE YAZ FROM BAS1. 
           WRITE YAZ FROM BAS2. WRITE YAZ FROM BAS3.
           WRITE YAZ FROM BAS5.
       K-YN-5. READ KON2 NEXT AT END GO K-YN-SONN.
       K-YN-6. IF C-KOD NOT =  KOD11 GO K-YN-5.
           ADD 1 TO KSIRA. ADD 1 TO SIRA. MOVE SIRA TO SR.
           MOVE C-KOD TO M-KOD. MOVE UNVAN1 TO M-UNVAN.
           MOVE ADRES TO M-ADRES. MOVE I-GUNU TO Y-GUN.
           MOVE I-AYI TO Y-AY. MOVE I-YILI TO Y-YIL.
           MOVE I-NO TO Y-NO. MOVE MAL-ISMI TO Y-MAL.
           MOVE TUTAR TO TUT. ADD TUTAR TO TUT-S.
           ADD TUTAR TO TUT-G.
           IF KSIRA = 1 PERFORM K-YN-BAS.
           WRITE YAZ FROM BAS4.
           IF KSIRA = 45 MOVE TUT-S TO SY-TOP MOVE TUT-G TO GY-TOP
           WRITE YAZ FROM BAS5 WRITE YAZ FROM BAS6
           WRITE YAZ FROM BAS7 WRITE YAZ FROM BOSLUK
           DISPLAY (24 1) ERASE DISPLAY (24 1)
           "D�K�ME DEVAM ETMEK �ST�YORMUSUNUZ" ACCEPT (24 45) C
           IF C = "H" GO K-YN-2 ELSE MOVE 0 TO LIN.
           GO K-YN-5.
       K-YN-SONN. IF KSIRA > 0 MOVE TUT-S TO SY-TOP MOVE TUT-G TO GY-TOP
           WRITE YAZ FROM BAS5 WRITE YAZ FROM BAS6 WRITE YAZ FROM BAS7
           WRITE YAZ FROM BOSLUK.
           DISPLAY (24 1) ERASE. DISPLAY (24 1) "ANA MENU ���N ENT BAS".
           ACCEPT C. CLOSE KON2 KON1 YAZICI.
       K-P-SON SECTION.
       MEHMET-HARAS. DISPLAY (1 1) ERASE. STOP RUN.
