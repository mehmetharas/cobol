      * BU PROGRAM ZEYTIN ALIM ISLEMLERI YAPILDIKTAN SONRA 
      * DONEME AIT TOPLAM ALIMLARI DOKEN VE ORTALAMALARI DOKER 
      * GRUP DOKUMU ISE SADECE O GUNKU ALIMLARI GRUP GRUP DOKER
      * FIS KESIMI ISE GUNLUK ALIMLARA FIS KESILMESINI SAGLAR.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. P2.
       AUTHOR. MEHMET HARAS.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BRD ASSIGN TO DISK ORGANIZATION INDEXED
           ACCESS DYNAMIC RECORD KEY KOD.
           SELECT YAZICI ASSIGN TO PRINTER.
       DATA DIVISION.
       FILE SECTION.
       FD BRD LABEL RECORD STANDARD VALUE OF FILE-ID "BRD.DAT".
       01 BRD-KAYDI.
           02 KOD.
              03 FISNO PIC 9(6).
              03 KALITE PIC X(6).
              03 DONEM PIC X(5).
              03 TARIH PIC X(10).
           02 UYENO PIC 9(4).
           02 AD-SOYAD PIC X(20).
           02 TANE PIC 9(4).
           02 BAREM-FIATI PIC 9(6).
           02 MIKTARI PIC 9(4).
           02 STNO PIC 9(3).
       FD YAZICI LABEL RECORD OMITTED.
       01 SATIR PIC X(80).
       WORKING-STORAGE SECTION.
       77 K PIC 9 VALUE 0.
       77 CEVAP PIC X VALUE SPACES.
       77 Z PIC 9(7) VALUE 0.
       77 ZZZ PIC 9(7) VALUE 0.
       77 BBA1 PIC 9(7) VALUE 0.
       77 BBA2 PIC 9(7) VALUE 0.
       77 BBA3 PIC 9(7) VALUE 0.
       77 BBA4 PIC 9(7) VALUE 0. 
       77 BBA5 PIC 9(7) VALUE 0.
       77 BBA6 PIC 9(7) VALUE 0.
       77 CC1 PIC 9 VALUE 0.
       77 SY PIC 9(6) VALUE 0.
       77 SY1 PIC 9(6) VALUE 0.
       77 MEH PIC 9(11) VALUE 0.
       77 T1 PIC X(10) VALUE SPACES.
       77 D1 PIC X(5) VALUE SPACES.
       77 K1 PIC X(6) VALUE SPACES.
       77 QP PIC 9 VALUE 0.
       77 C PIC 9 VALUE 0.
       77 SC PIC 99 VALUE 0.
       77 TUTAR PIC 9(8) VALUE 0.
       77 SIFRE PIC X(10) VALUE SPACES.
       77 SIRA PIC 99 VALUE 0.
       77 RC PIC 9 VALUE 0.
       77 SN1 PIC 999 VALUE 0.
       77 SN2 PIC 999 VALUE 0.
       77 SN3 PIC 999 VALUE 0.
       77 SN4 PIC 999 VALUE 0.
       77 SN5 PIC 999 VALUE 0.
       77 SN6 PIC 999 VALUE 0.
       77 BBBL PIC 9(7) VALUE 0.
       77 BBL1 PIC 9(7) VALUE 0.
       77 BBL2 PIC 9(7) VALUE 0.
       77 BBL3 PIC 9(7) VALUE 0.
       77 BBL4 PIC 9(7) VALUE 0.
       77 BBL5 PIC 9(7) VALUE 0.
       77 BBL6 PIC 9(7) VALUE 0.
       77 BBL PIC 9(7) VALUE 0.
       77 B1 PIC 9(6) VALUE 0.
       77 B2 PIC 9(6) VALUE 0.
       77 B3 PIC 9(6) VALUE 0.
       77 B4 PIC 9(6) VALUE 0.
       77 B5 PIC 9(6) VALUE 0.
       77 B6 PIC 9(6) VALUE 0.
       77 B7 PIC 9(6) VALUE 0.
       77 B8 PIC 9(6) VALUE 0.
       77 B9 PIC 9(6) VALUE 0. 
       77 B10 PIC 9(6) VALUE 0.
       77 GORT PIC 9999V99.
       77 GTORT PIC 9999V99.
       77 B11 PIC 9(6) VALUE 0.
       77 B12 PIC 9(6) VALUE 0. 
       77 B13 PIC 9(6) VALUE 0.
       77 B14 PIC 9(6) VALUE 0.
       77 B15 PIC 9(6) VALUE 0.
       77 B16 PIC 9(6) VALUE 0.
       77 B17 PIC 9(6) VALUE 0.
       77 B18 PIC 9(6) VALUE 0.
       77 B19 PIC 9(6) VALUE 0.
       77 B20 PIC 9(6) VALUE 0.
       77 B21 PIC 9(6) VALUE 0.
       77 B22 PIC 9(6) VALUE 0.
       77 B23 PIC 9(6) VALUE 0.
       77 B24 PIC 9(6) VALUE 0.
       77 B25 PIC 9(6) VALUE 0.
       77 B26 PIC 9(6) VALUE 0.
       77 B27 PIC 9(6) VALUE 0.
       77 B28 PIC 9(6) VALUE 0.
       77 B29 PIC 9(6) VALUE 0.
       77 YYY PIC 9(7) VALUE 0.
       77 RRR PIC 9(7) VALUE 0.
       77 BL1 PIC 9(7) VALUE 0.
	   77 BL2 PIC 9(7) VALUE 0.
       77 BL3 PIC 9(7) VALUE 0.
       77 BL4 PIC 9(7) VALUE 0.
       77 BL5 PIC 9(7) VALUE 0.
       77 BL6 PIC 9(7) VALUE 0.
       77 BL7 PIC 9(7) VALUE 0.
       77 BL8 PIC 9(7) VALUE 0.
       77 BL9 PIC 9(7) VALUE 0.
       77 BL10 PIC 9(7) VALUE 0.
       77 BL11 PIC 9(7) VALUE 0.
       77 BL12 PIC 9(7) VALUE 0.
       77 BL13 PIC 9(7) VALUE 0.
       77 BL14 PIC 9(7) VALUE 0.
       77 BL15 PIC 9(7) VALUE 0.
       77 BL16 PIC 9(7) VALUE 0.
       77 BL17 PIC 9(7) VALUE 0.
       77 BL18 PIC 9(7) VALUE 0.    
       77 BL19 PIC 9(7) VALUE 0.
       77 BL20 PIC 9(7) VALUE 0.
       77 BL21 PIC 9(7) VALUE 0.
       77 BL22 PIC 9(7) VALUE 0.
       77 BL23 PIC 9(7) VALUE 0.
       77 BL24 PIC 9(7) VALUE 0.
       77 BL25 PIC 9(7) VALUE 0.
       77 BL26 PIC 9(7) VALUE 0.
       77 BL27 PIC 9(7) VALUE 0.
       77 BL28 PIC 9(7) VALUE 0.
       77 BL29 PIC 9(7) VALUE 0.
       77 GT1 PIC 9(7) VALUE 0.
       77 GT2 PIC 9(7) VALUE 0.
       77 GT3 PIC 9(7) VALUE 0.
       77 GT4 PIC 9(7) VALUE 0.
       77 GT5 PIC 9(7) VALUE 0.
       77 GT6 PIC 9(7) VALUE 0.
       77 GTT PIC 9(7) VALUE 0.
       77 GTT1 PIC 9(7) VALUE 0.
       77 GTT2 PIC 9(7) VALUE 0.
       77 GTT3 PIC 9(7) VALUE 0.
       77 GTT4 PIC 9(7) VALUE 0.
       77 GTT5 PIC 9(7) VALUE 0.
       77 GTT6 PIC 9(7) VALUE 0.
       77 GTTT PIC 9(7) VALUE 0.
       77 BOT1 PIC 9(7) VALUE 0.
       77 BOT2 PIC 9(7) VALUE 0.
       77 BOT3 PIC 9(7) VALUE 0.
       77 BOT4 PIC 9(7) VALUE 0.
       77 BOT5 PIC 9(7) VALUE 0.
       77 BOT6 PIC 9(7) VALUE 0.
       77 BOTT PIC 9(7) VALUE 0.
       77 BA1 PIC 9(7) VALUE 0.
       77 BA2 PIC 9(7) VALUE 0.
       77 BA3 PIC 9(7) VALUE 0.
       77 BA4 PIC 9(7) VALUE 0. 
       77 BA5 PIC 9(7) VALUE 0.
       77 BA6 PIC 9(7) VALUE 0.
       77 BOSLUK PIC X(80) VALUE SPACES.
       77 CIZGI PIC X(80) VALUE ALL "~".
       01 N1.  
           02 F PIC X(15) VALUE SPACES.
           02 TT PIC X(10)BB.  
           02 F PIC X(17) VALUE "TARIHINDE ALINAN".
           02 KK PIC X(6)BB.
           02 F PIC X(20) VALUE "TIPI ZEYTINLER".
       01 N2.
           02 F PIC X(37) VALUE SPACES.
           02 F PIC X(5) VALUE "KOOP".     
           02 NC PIC 9.
       01 N3.
           02 F PIC X(15) VALUE SPACES.
           02 SSTN1 PIC 999.
           02 F PIC X(53) VALUE SPACES.
           02 SSTN2 PIC 999.
       01 N4.
           02 F PIC X(8) VALUE SPACES.
           02 F PIC X(53) VALUE "220-250".
           02 F PIC X(8) VALUE "251-300".
       01 N5.
           02 F PIC X VALUE SPACES.
           02 F PIC X(43) VALUE "220    230    240    250".
           02 F PIC X(36) VALUE "260    270    280    290    300".
           02 F PIC X(1) VALUE SPACES.
           02 F PIC X(79) VALUE ALL "-".
       01 N6.
           02 F PIC X VALUE SPACES.
           02 BB1 PIC ZZZ,ZZ9.
           02 F PIC X VALUE SPACES.
           02 BB2 PIC ZZZ,ZZ9.
           02 F PIC X VALUE SPACES.
           02 BB3 PIC ZZZ,ZZ9.
           02 F PIC X VALUE SPACES.
           02 BB4 PIC ZZZ,ZZ9.
           02 F PIC X(19) VALUE SPACES.
           02 BB5 PIC ZZZ,ZZ9.
           02 F PIC X VALUE SPACES.
           02 BB6 PIC ZZZ,ZZ9.        
           02 F PIC X VALUE SPACES.    
           02 BB7 PIC ZZZ,ZZ9.
           02 F PIC X VALUE SPACES.
           02 BB8 PIC ZZZ,ZZ9.
           02 F PIC X VALUE SPACES.
           02 BB9 PIC ZZZ,ZZ9.
       01 N7.
           02 F PIC X(15) VALUE SPACES.
           02 SSTN3 PIC 999.
           02 F PIC X(53) VALUE SPACES.
           02 SSTN4 PIC 999.
       01 N8.
           02 F PIC X(8) VALUE SPACES.
           02 F PIC X(53) VALUE "301-350".
           02 F PIC X(8) VALUE "351-400".
       01 N9.
           02 F PIC X VALUE SPACES.
           02 F PIC X(43) VALUE "310    320    330    340    350".
           02 F PIC X(36) VALUE "360    370    380    390    400".
           02 F PIC X(1) VALUE SPACES.
           02 F PIC X(78) VALUE ALL "-".
       01 N10.
           02 F PIC X VALUE SPACES.
           02 BB10 PIC ZZZ,ZZ9.
           02 F PIC X VALUE SPACES.
           02 BB11 PIC ZZZ,ZZ9.
           02 F PIC X VALUE SPACES.
           02 BB12 PIC ZZZ,ZZ9.
           02 F PIC X VALUE SPACES.
           02 BB13 PIC ZZZ,ZZ9.
           02 F PIC X VALUE SPACES.
		   02 BB14 PIC ZZZ,ZZ9.
           02 F PIC X(19) VALUE SPACES.
           02 BB15 PIC ZZZ,ZZ9.
           02 F PIC X VALUE SPACES.
           02 BB16 PIC ZZZ,ZZ9.
           02 F PIC X VALUE SPACES.
           02 BB17 PIC ZZZ,ZZ9.
           02 F PIC X VALUE SPACES.
           02 BB18 PIC ZZZ,ZZ9.
           02 F PIC X VALUE SPACES.
           02 BB19 PIC ZZZ,ZZ9.
       01 N11.
           02 F PIC X(15) VALUE SPACES.
           02 SSTN5 PIC 999.
           02 F PIC X(53) VALUE SPACES.
           02 SSTN6 PIC 999.
       01 N12.
           02 F PIC X(8) VALUE SPACES.
           02 F PIC X(53) VALUE "401-460".
           02 F PIC X(8) VALUE "461-500".
       01 N13.
           02 F PIC X VALUE SPACES.
           02 F PIC X(48) VALUE 
              "410    420    430    440    450    460".
           02 F PIC X(31) VALUE "470    480    490    500".
           02 F PIC X(1) VALUE SPACES.
           02 F PIC X(78) VALUE ALL "-".
       01 N14.
           02 F PIC X VALUE SPACES.
           02 BB20 PIC ZZZ,ZZ9.
           02 F PIC X VALUE SPACES.
           02 BB21 PIC ZZZ,ZZ9.
           02 F PIC X VALUE SPACES.
           02 BB22 PIC ZZZ,ZZ9.
           02 F PIC X VALUE SPACES.
           02 BB23 PIC ZZZ,ZZ9.
           02 F PIC X VALUE SPACES.
		   02 BB24 PIC ZZZ,ZZ9.
           02 F PIC X VALUE SPACES.
           02 BB25 PIC ZZZ,ZZ9.
           02 F PIC X(19) VALUE SPACES.
           02 BB26 PIC ZZZ,ZZ9.
           02 F PIC X VALUE SPACES.
           02 BB27 PIC ZZZ,ZZ9.
           02 F PIC X VALUE SPACES.
           02 BB28 PIC ZZZ,ZZ9.
           02 F PIC X VALUE SPACES.
           02 BB29 PIC ZZZ,ZZ9.
       01 N15.
           02 F PIC X(40) VALUE SPACES.
           02 F PIC X(19) VALUE "TOPLAM KILO :".
           02 TPKL1 PIC Z,ZZZ,ZZ9.
       01 N16.
           02 F PIC X(40) VALUE SPACES.
           02 F PIC X(15) VALUE "TOPLAM FIATI:".
           02 TPBDL1 PIC ZZ,ZZZ,ZZZ,ZZ9.
       01 M1.
           02 F PIC X(10) VALUE SPACES.
           02 F PIC X(50) VALUE "MARMARA ZEYTIN T.S.". 
           02 F PIC X(15) VALUE "DIP KOCAN".
       01 M2.
           02 F PIC X(15) VALUE SPACES.
           02 F PIC X(45) VALUE "KOOPERATIFLERI BIRLIGI".
           02 F PIC X(6) VALUE ALL "-".
       01 M3.
           02 F PIC X(5) VALUE SPACES.
           02 F PIC X(45) VALUE "EDINCIK KOOPERATIFI DEPO GIRIS FISI".
           02 F PIC X(3) VALUE "NO:".
           02 SYF2 PIC ZZZ,ZZ9.
       01 M4.
           02 F PIC X(5) VALUE SPACES.
           02 F PIC X(15) VALUE "DEPO ADI: KOOP".
           02 NC1 PIC 9.
           02 F PIC X(28) VALUE SPACES.
           02 F PIC X(10) VALUE "TARIH:".
           02 TT1 PIC X(10).
       01 M5.
           02 F PIC X(5) VALUE SPACES.
           02 F PIC X(20) VALUE "TESELLUM MEMURLUGU".
           02 F PIC X(14) VALUE "ALIM YERI".
       01 M6.
           02 F PIC XXX VALUE SPACES.
           02 F PIC X(76) VALUE ALL "-".
       01 M7.
           02 F PIC X(3) VALUE SPACES.
           02 F PIC X(34) VALUE "|EMTIANIN CINSI|EMTIANIN KALITESI|".
           02 F PIC X(32) VALUE "KAP.NO|SAFI MIKTAR|BEDELI      |".
       01 M8.
           02 F PIC XXX VALUE SPACES.
           02 F PIC X(75) VALUE ALL "-".
       01 M9.
           02 F PIC XXX VALUE SPACES.
           02 F PIC XXX VALUE "|K.".
           02 KK1 PIC X(6)B.
           02 F PIC X(24) VALUE 
		     "TIPI |220-250          |". 
           02 F PIC X(2) VALUE SPACES.
           02 STN1 PIC 999.
           02 F PIC X(3) VALUE " | ".
           02 AB1 PIC Z,ZZZ,ZZ9.
           02 F PIC XXXX VALUE "  | ".
           02 BBBL1 PIC Z,ZZZ,ZZ9.
       01 M10.
           02 F PIC X(18) VALUE SPACES.
           02 F PIC X(19) VALUE 
	         "|251-300          |".
           02 F PIC X(2) VALUE SPACES.
           02 STN2 PIC 999.
           02 F PIC X VALUE "|".
           02 AB2 PIC Z,ZZZ,ZZ9.
           02 F PIC XXXX VALUE "  | ".
           02 BBBL2 PIC Z,ZZZ,ZZ9.
       01 M11.
           02 F PIC X(18) VALUE SPACES.
           02 F PIC X(19) VALUE 
             "|301-350          |".
           02 F PIC X(2) VALUE SPACES.
           02 STN3 PIC 999.
           02 F PIC X VALUE "|".
           02 AB3 PIC Z,ZZZ,ZZ9.
           02 F PIC XXXX VALUE "  | ".
           02 BBBL3 PIC Z,ZZZ,ZZ9.
       01 M12.
           02 F PIC X(18) VALUE SPACES.
           02 F PIC X(19) VALUE 
             "|351-400          |".
           02 F PIC X(2) VALUE SPACES.
           02 STN4 PIC 999.
           02 F PIC X VALUE "|".
           02 AB4 PIC Z,ZZZ,ZZ9.
           02 F PIC X(6) VALUE "  | ".
           02 BBBL4 PIC Z,ZZZ,ZZ9.
       01 M13.
           02 F PIC X(18) VALUE SPACES.
           02 F PIC X(19) VALUE 
             "|401-460          |".
           02 F PIC X(2) VALUE SPACES.
           02 STN5 PIC 999.
           02 F PIC X VALUE "|".       
           02 AB5 PIC Z,ZZZ,ZZ9.
           02 F PIC X(6) VALUE "  | ". 
           02 BBBL5 PIC Z,ZZZ,ZZ9.
       01 M14.
           02 F PIC X(18) VALUE SPACES.
           02 F PIC X(19) VALUE 
             "|461-500          |".
           02 F PIC X(2) VALUE SPACES.
           02 STN6 PIC 999.
           02 F PIC X VALUE "|".
		   02 AB6 PIC Z,ZZZ,ZZ9.
           02 F PIC X(6) VALUE "  | ".
           02 BBBL6 PIC ZZZ,ZZZ,ZZ9.
       01 M15.
           02 F PIC X(47) VALUE SPACES.
           02 F PIC X(30) VALUE ALL "-".
       01 M16.
           02 F PIC X(30) VALUE SPACES.
           02 F PIC X(17) VALUE "TOPLAM:".
           02 TPKL PIC Z,ZZZ,ZZ9.
           02 F PIC XX VALUE " |".
           02 TPBDL PIC ZZ,ZZZ,ZZZ,ZZ9.
       01 M17.
           02 F PIC XXX VALUE SPACES.
           02 F PIC X(75) VALUE ALL "-".
       01 DZ1.
           02 F PIC X(30) VALUE "775 SAYILI EDINCIK".
           02 DD1 PIC X(6)B.
           02 F PIC X(12) VALUE "KAMPANYASI".
       01 DZ2.
           02 F PIC X(30) VALUE "ZEYTIN T.S. KOOPERATIFI".
           02 F PIC X(25) VALUE "GUNLUK ALIM RAPORU".
       01 DZ3.
           02 F PIC X(30) VALUE SPACES.
           02 KKALITE PIC X(6).
           02 F PIC X(25) VALUE  "    TIPI ZEYTINLER".
           02 F PIC X(7) VALUE "TARIH:".
           02 TTARIH PIC X(10).
       01 DZ4.
           02 F PIC X(30) VALUE SPACES.
           02 F PIC X(25) VALUE "A. ALIM MERKEZINE GORE".
           02 F PIC X(3) VALUE "NO:".
           02 SYF1 PIC ZZZ,ZZ9.
       01 DZ5.
           02 F PIC X(80) VALUE ALL "-".
       01 DZ6.
           02 F PIC X(9) VALUE "A.MERKEZI".
           02 F PIC X(27) VALUE "BUGUNKU ALIMLAR".
           02 F PIC X(40) VALUE "TOPLAM ALIMLAR".
       01 DZ7.
           02 F PIC X(9) VALUE SPACES.
           02 F PIC X(32) VALUE 
              "| KILOSU | TUTARI     |ORT.FIAT|".  
           02 F PIC X(39) VALUE 
              "| KILOSU   | TUTAR         |T.OR.FIAT |".
       01 DZ8.
           02 F PIC X(9) VALUE "EDINCIK".
           02 MTPKL PIC Z,ZZZ,ZZ9.
           02 F PIC XX VALUE SPACES.
           02 MTPBDL PIC ZZ,ZZZ,ZZZ9.
           02 F PIC XX VALUE SPACES.
           02 ORF PIC Z,ZZ9.99.
           02 F PIC XX VALUE SPACES.
           02 ATPKL PIC Z,ZZZ,ZZ9.
           02 F PIC XX VALUE SPACES.
           02 ATPBDL PIC ZZ,ZZZ,ZZZ,ZZ9.
           02 TORF PIC Z,ZZ9.99.
       01 DZ9.
           02 F PIC X(80) VALUE SPACES.
           02 F PIC X(80) VALUE SPACES.
           02 F PIC X(80) VALUE ALL "-".
           02 F PIC X(30) VALUE SPACES.
           02 F PIC X(50) VALUE "B. TANE SAYILARINA GORE".
           02 F PIC X(80) VALUE ALL "-".
           02 F PIC X(35) VALUE SPACES.
           02 F PIC X(22) VALUE "BAREM ICI".
           02 F PIC X(23) VALUE SPACES.
           02 F PIC X(80) VALUE ALL "-".
       01 DZ10.
           02 F PIC X(7) VALUE SPACES.
           02 F PIC X(62) VALUE
             "|TANE     |TANE     |TANE     |TANE     |TANE     |TANE".
           02 F PIC X(8) VALUE "|TOPLAM".
       01 DZ11.
           02 F PIC X(7) VALUE SPACES.
           02 F PIC X(63) VALUE 
           "|220-250  |251-300  |301-350  |351-400  |401-460  |461-500".
       01 DZ12.
           02 F PIC X(10) VALUE SPACES.
           02 F PIC X(70) VALUE ALL "-".
       01 DZ13.
           02 F PIC X(8) VALUE "B.ALIM|".
           02 HAB1 PIC Z,ZZZ,ZZ9.
           02 F PIC X VALUE "|".
           02 HAB2 PIC Z,ZZZ,ZZ9.
           02 F PIC X VALUE "|".
           02 HAB3 PIC Z,ZZZ,ZZ9.
           02 F PIC X VALUE "|".
           02 HAB4 PIC Z,ZZZ,ZZ9.
           02 F PIC X VALUE "|".
           02 HAB5 PIC Z,ZZZ,ZZ9.
           02 F PIC X VALUE "|".
           02 HAB6 PIC Z,ZZZ,ZZ9.
           02 F PIC X VALUE "|".
           02 HTPKL PIC Z,ZZZ,ZZ9.
           02 F PIC X VALUE "|".
       01 DZ14.
           02 F PIC X(8) VALUE "B.O.A.|".
           02 DAB1 PIC Z,ZZZ,ZZ9.
           02 F PIC X VALUE "|".
           02 DAB2 PIC Z,ZZZ,ZZ9.
           02 F PIC X VALUE "|".
           02 DAB3 PIC Z,ZZZ,ZZ9.
           02 F PIC X VALUE "|".
           02 DAB4 PIC Z,ZZZ,ZZ9.
           02 F PIC X VALUE "|". 
           02 DAB5 PIC Z,ZZZ,ZZ9.
           02 F PIC X VALUE "|".
           02 DAB6 PIC Z,ZZZ,ZZ9.
           02 F PIC X VALUE "|".
           02 DTPKL PIC Z,ZZZ,ZZ9.
           02 F PIC X VALUE "|".
       01 DZ15.
           02 F PIC X(8) VALUE "G.T.A.|".
           02 ZAB1 PIC Z,ZZZ,ZZ9.
           02 F PIC X VALUE "|". 
           02 ZAB2 PIC Z,ZZZ,ZZ9.
           02 F PIC X VALUE "|".
           02 ZAB3 PIC Z,ZZZ,ZZ9.
           02 F PIC X VALUE "|".
           02 ZAB4 PIC Z,ZZZ,ZZ9.
           02 F PIC X VALUE "|".
           02 ZAB5 PIC Z,ZZZ,ZZ9.
           02 F PIC X VALUE "|".
           02 ZAB6 PIC Z,ZZZ,ZZ9.
           02 F PIC X VALUE "|".
           02 ZTPKL PIC Z,ZZZ,ZZ9.
           02 F PIC X VALUE "|".
       SCREEN SECTION.   
       01 EK1.
           02 BLANK SCREEN.
           02 LINE 1 COLUMN 1 VALUE "MARMARA BIRLIK".
           02 LINE 2 COLUMN 1 VALUE "ANA MENU".
           02 LINE 3 COLUMN 1 PIC X(10) FROM ALL "~".
           02 LINE 5 COLUMN 10 PIC X(60) FROM ALL "~".
           02 LINE 6 COLUMN 10 VALUE 
              "TOPLAM ALIMLARIN".
           02 LINE 7 COLUMN 10 VALUE "{1:EKRAN *** 2:YAZICI DOKUMU}".
           02 LINE 8 COLUMN 10 VALUE 
              "GUNLUK TOPLAM ALIMLARIN".
           02 LINE 9 COLUMN 10 VALUE "{3:EKRAN *** 4:YAZICI DOKUMU}".
           02 LINE 10 COLUMN 10 VALUE 
              "GUNLUK GIRIS FISI KESIMI".
           02 LINE 11 COLUMN 10 VALUE  "{5:EKRAN *** 6:YAZICI DOKUMU}".
		   02 LINE 12 COLUMN 10 VALUE "{7:CIKIS}".
           02 LINE 13 COLUMN 10 PIC X(60) FROM ALL "~".
           02 LINE 14 COLUMN 10 VALUE 
               "*** S E C I M I N I Z  [    ] ***".  
           02 LINE 14 COLUMN 35 PIC 99 USING SC.
       01 EK5.
           02 BLANK SCREEN.
           02 LINE 1 COLUMN 1 VALUE "MARMARA BIRLIK".
           02 LINE 5 COLUMN 10 PIC X(40) FROM ALL "~".
           02 LINE 6 COLUMN 10 VALUE "KALITE   :".
           02 LINE 7 COLUMN 10 VALUE "DONEM    :".
           02 LINE 8 COLUMN 10 VALUE "TARIH    :".
           02 LINE 6 COLUMN 30 PIC X(6) USING K1.
           02 LINE 7 COLUMN 30 PIC X(5) USING D1.
		   02 LINE 8 COLUMN 30 PIC X(10) USING T1.
       01 SIF. 02 LINE 12 COLUMN 28 PIC X(10) SECURE USING SIFRE.
       PROCEDURE DIVISION.
       MMMM. DISPLAY (1 1) ERASE.
           DISPLAY (11 10) "*****************************************".
           DISPLAY (12 10) "*** S I F R E :                       ***". 
           DISPLAY (13 10) "*****************************************".
           ACCEPT SIF ON ESCAPE GO ZZQQQ.
           IF SIFRE NOT = "EDINCIK" DISPLAY (1 1) ERASE STOP RUN.
       ZZQQQ. DISPLAY EK1.
       NNNN. ACCEPT EK1. 
           IF SC = 01 PERFORM GAE
            ELSE IF SC = 02 PERFORM GAE 
            ELSE IF SC = 03 PERFORM GAE 
            ELSE IF SC = 04 PERFORM GAE
            ELSE IF SC = 05 PERFORM GAE 
            ELSE IF SC = 06 PERFORM GAE
		    ELSE IF SC = 07 PERFORM SON
            ELSE DISPLAY (23 7) SC (23 10)
            "NOLU SECENEK YANLIS LUTFEN DOGRUSUNU GIRINIZ" GO NNNN.
       NNN1. DISPLAY EK1. DISPLAY (23 7) SC (23 10) 
           "NOLU ISLEM TAMAMLANMISTIR". GO NNNN.
       GAE SECTION. 
	   ASMR. DISPLAY (1 1) ERASE. 
       AOPE. OPEN I-O BRD. 
       AERTY. OPEN OUTPUT YAZICI.
	   SDSD. MOVE ALL "." TO K1 D1 T1. MOVE 0 TO FISNO SY SY1.
           MOVE 0 TO GT1 GT2 GT3 GT4 GT5 GT6 GTT GTT1 GTT2 GTT3 GTT4.
           MOVE 0 TO GTT5 GTT6 GTTT BOT1 BOT2 BOT3 BOT4 BOT5 BOT6 BOTT. 
           MOVE 0 TO MEH.
       ZAQ. DISPLAY EK5. IF SC = 1 IF SC = 3 IF SC = 5 
            DISPLAY (4 10) "EKRAN DOKUMU" ELSE IF SC = 2 IF SC = 4 
            IF SC = 6 DISPLAY (4 10) "YAZICI DOKUMU". 
           DISPLAY (23 1) "CTRL-B ILE BIR ALAN GERI GIDILIR". 
           DISPLAY (24 1) "ESC TUSU ILE ANA MENUYE DONERSINIZ".
           ACCEPT EK5 ON ESCAPE GO PROJEQQ. 
           DISPLAY (22 10) "DOGRUMU {E/H}:". 
           ACCEPT (22 30) CEVAP. 
           IF CEVAP = "H" GO ZAQ. 
           MOVE K1 TO KALITE KK KK1. 
		   MOVE D1 TO DONEM DD1. 
           MOVE T1 TO TARIH TT TT1 TTARIH.
       SS1. IF K1 = "GEMLIK" ADD 1 TO SY MOVE SY TO SYF2 SYF1
           MOVE 3 TO NC NC1
           ELSE ADD 1 TO SY1 MOVE SY1 TO SYF1 SYF2 MOVE 2 TO NC NC1.
       SS2. START BRD KEY NOT LESS THAN KOD INVALID KEY  
            DISPLAY (23 1) ERASE DISPLAY (23 1)
            "DOSYAYA START YAPILAMIYOR {ENT BAS}"
            ACCEPT CEVAP GO PROJEQQ. MOVE 0 TO BBL.
	   HS1. READ BRD NEXT AT END GO HSS1.	   
           IF K1 = KALITE AND 
            D1 = DONEM AND 
            T1 = TARIH GO HS2. GO HS1.
       HS2. MOVE 0 TO BA1 BA2 BA3 BA4 BA5 BA6 BBL BBL1 BBL2 BBL3 BBL4.
           MOVE 0 TO BBL5 BBL6 Z. 
           IF TANE = 220 ADD MIKTARI TO B1   
	        MOVE STNO TO STN1 SSTN1
            COMPUTE BL1 = MIKTARI * BAREM-FIATI
            ELSE IF TANE = 230 ADD MIKTARI TO B2
            MOVE STNO TO STN1 SSTN1
            COMPUTE BL2 = MIKTARI * BAREM-FIATI
            ELSE IF TANE = 240 ADD MIKTARI TO B3 
            MOVE STNO TO STN1 SSTN1
            COMPUTE BL3 = MIKTARI * BAREM-FIATI
            ELSE IF TANE = 250 ADD MIKTARI TO B4 
            MOVE STNO TO STN1 SSTN1
            COMPUTE BL4 = MIKTARI * BAREM-FIATI
            ELSE IF TANE = 260 ADD MIKTARI TO B5 
            MOVE STNO TO STN2 SSTN2
            COMPUTE BL5 = MIKTARI * BAREM-FIATI
            ELSE IF TANE = 270 ADD MIKTARI TO B6 
            MOVE STNO TO STN2 SSTN2
            COMPUTE BL6 = MIKTARI * BAREM-FIATI
            ELSE IF TANE = 280 ADD MIKTARI TO B7 
            MOVE STNO TO STN2 SSTN2
            COMPUTE BL7 = MIKTARI * BAREM-FIATI
            ELSE IF TANE = 290 ADD MIKTARI TO B8 
            MOVE STNO TO STN2 SSTN2
            COMPUTE BL8 = MIKTARI * BAREM-FIATI
            ELSE IF TANE = 300 ADD MIKTARI TO B9 
            MOVE STNO TO STN2 SSTN2
            COMPUTE BL9 = MIKTARI * BAREM-FIATI
            ELSE IF TANE = 310 ADD MIKTARI TO B10 
            MOVE STNO TO STN3 SSTN3
            COMPUTE BL10 = MIKTARI * BAREM-FIATI
            ELSE IF TANE = 320 ADD MIKTARI TO B11 
            MOVE STNO TO STN3 SSTN3
            COMPUTE BL11 = MIKTARI * BAREM-FIATI
            ELSE IF TANE = 330 ADD MIKTARI TO B12 
            MOVE STNO TO STN3 SSTN3
            COMPUTE BL12 = MIKTARI * BAREM-FIATI
            ELSE IF TANE = 340 ADD MIKTARI TO B13 
            MOVE STNO TO STN3 SSTN3
            COMPUTE BL13 = MIKTARI * BAREM-FIATI
            ELSE IF TANE = 350 ADD MIKTARI TO B14 
            MOVE STNO TO STN3 SSTN3
            COMPUTE BL14 = MIKTARI * BAREM-FIATI
            ELSE IF TANE = 360 ADD MIKTARI TO B15 
            MOVE STNO TO STN4 SSTN4
            COMPUTE BL15 = MIKTARI * BAREM-FIATI
            ELSE IF TANE = 370 ADD MIKTARI TO B16 
            MOVE STNO TO STN4 SSTN4
            COMPUTE BL16 = MIKTARI * BAREM-FIATI
            ELSE IF TANE = 380 ADD MIKTARI TO B17 
            MOVE STNO TO STN4 SSTN4
            COMPUTE BL17 = MIKTARI * BAREM-FIATI
            ELSE IF TANE = 390 ADD MIKTARI TO B18 
            MOVE STNO TO STN4 SSTN4
            COMPUTE BL18 = MIKTARI * BAREM-FIATI
            ELSE IF TANE = 400 ADD MIKTARI TO B19 
            MOVE STNO TO STN4 SSTN4
            COMPUTE BL19 = MIKTARI *BAREM-FIATI
            ELSE IF TANE = 410 ADD MIKTARI TO B20 
            MOVE STNO TO STN5 SSTN5
            COMPUTE BL20 = MIKTARI * BAREM-FIATI
            ELSE IF TANE = 420 ADD MIKTARI TO B21 
            MOVE STNO TO STN5 SSTN5
            COMPUTE BL21 = MIKTARI * BAREM-FIATI
            ELSE IF TANE = 430 ADD MIKTARI TO B22 
            MOVE STNO TO STN5 SSTN5
            COMPUTE BL22 = MIKTARI * BAREM-FIATI
            ELSE IF TANE = 440 ADD MIKTARI TO B23  
            MOVE STNO TO STN5 SSTN5
            COMPUTE BL23 = MIKTARI * BAREM-FIATI
            ELSE IF TANE = 450 ADD MIKTARI TO B24  
            MOVE STNO TO STN5 SSTN5
            COMPUTE BL24 = MIKTARI * BAREM-FIATI
            ELSE IF TANE = 460 ADD MIKTARI TO B25 
            MOVE STNO TO STN5 SSTN5
            COMPUTE BL25 = MIKTARI * BAREM-FIATI
            ELSE IF TANE = 470 ADD MIKTARI TO B26 
            MOVE STNO TO STN6 SSTN6
            COMPUTE BL26 = MIKTARI * BAREM-FIATI
            ELSE IF TANE = 480 ADD MIKTARI TO B27 
            MOVE STNO TO STN6 SSTN6
            COMPUTE BL27 = MIKTARI * BAREM-FIATI
            ELSE IF TANE = 490 ADD MIKTARI TO B28 
            MOVE STNO TO STN6 SSTN6
            COMPUTE BL28 = MIKTARI * BAREM-FIATI
            ELSE IF TANE = 500 ADD MIKTARI TO B29 
            MOVE STNO TO STN6 SSTN6
           COMPUTE BA1 = B1 + B2 + B3 + B4.
           COMPUTE BA2 = B5 + B6 + B7 + B8 + B9.
           COMPUTE BA3 = B10 + B11 + B12 + B13 + B14. MOVE B1 TO BB1.
           COMPUTE BA4 = B15 + B16 + B17 + B18 + B19. 
           MOVE BA4 TO AB4 HAB4.
           COMPUTE BA5 = B20 + B21 + B22 + B23 + B24 + B25. 
           MOVE BA5 TO AB5 HAB5.
           COMPUTE BA6 = B26 + B27 + B28 + B29.  
           MOVE BA6 TO AB6 HAB6.
           COMPUTE BBL1 = BL1 + BL2 + BL3 + BL4.  MOVE B2 TO BB2.
           COMPUTE BBL2 = BL5 + BL6 + BL7 + BL8 + BL9.  MOVE B3 TO BB3.
           COMPUTE BBL3 = BL10 + BL11 + BL12 + BL13 + BL14. 
           MOVE B4 TO BB4. 
		   MOVE BBL3 TO BBBL3. 
           COMPUTE BBL4 = BL15 + BL16 + BL17 + BL18 + BL19.  
           MOVE B5 TO BB5.
           MOVE BBL4 TO BBBL4.
           COMPUTE BBL5 = BL20 + BL21 + BL22 + BL23 + BL24 + BL25.
           MOVE BBL5 TO BBBL5.
           COMPUTE BBL6 = BL26 + BL27 + BL28 + BL29. 
		   MOVE BBL6 TO BBBL6. 
           MOVE B6 TO BB6.
           MOVE BA1 TO AB1 HAB1. MOVE BA2 TO AB2 HAB2. 
           MOVE BA3 TO AB3 HAB3.
           COMPUTE BBL = BBL1 + BBL2 + BBL3 + BBL4 + BBL5 + BBL6. 
           MOVE B7 TO BB7. MOVE BBL1 TO BBBL1. MOVE BBL2 TO BBBL2.
           COMPUTE Z = BA1 + BA2 + BA3 + BA4 + BA5 + BA6.
           MOVE B8 TO BB8. 
		   MOVE B9 TO BB9.
           MOVE B10 TO BB10.  MOVE B11 TO BB11.  MOVE B12 TO BB12.
           MOVE B13 TO BB13. MOVE B14 TO BB14. MOVE B15 TO BB15.
           MOVE B16 TO BB16. MOVE B17 TO BB17. MOVE B18 TO BB18.
           MOVE B19 TO BB19. MOVE B20 TO BB20. MOVE B21 TO BB21.
           MOVE B22 TO BB22. MOVE B23 TO BB23. MOVE B24 TO BB24.           
           MOVE B25 TO BB25. MOVE B26 TO BB26. MOVE B27 TO BB27.
           MOVE B28 TO BB28. MOVE B29 TO BB29.
       SSMN. GO HS1.
       HSS1. MOVE 0 TO BBBL BBA1 BBA2 BBA3 BBA4 BBA5 BBA6 YYY GORT ZZZ. 
           MOVE BBL TO TPBDL TPBDL1 GTT. ADD BBL TO MEH. 
           MOVE Z TO ZZZ. 
           MOVE ZZZ TO TPKL TPKL1 MTPKL HTPKL. 
           MOVE BBL TO MTPBDL BBBL.
           COMPUTE GORT = BBL / Z. MOVE BA1 TO BBA1 GT1.
           MOVE BA2 TO BBA2 GT2. MOVE BA3 TO GT3 BBA3. 
           MOVE BA4 TO GT4 BBA4. MOVE BA5 TO GT5 BBA5. 
	       MOVE BA6 TO GT6 BBA6. MOVE GORT TO ORF.
       ANMA. IF SC = 3 AND SC = 4 GO AQP1. 
           IF SC = 5 AND SC = 6 GO AQP2.
       ISLEM2. CLOSE BRD. 
           OPEN I-O BRD.
           PERFORM SIFIRLAMA. MOVE 0 TO FISNO.
           MOVE ALL "." TO KALITE DONEM TARIH.
           MOVE K1 TO KALITE. MOVE T1 TO TARIH. MOVE D1 TO DONEM.
           START BRD KEY NOT LESS THAN KOD INVALID KEY
           DISPLAY (23 1) ERASE DISPLAY (23 1)
           "DOSYAYA START YAPILAMIYOR {ENT BAS}" 
           ACCEPT CEVAP GO PROJEQQ.
   	   HS3. READ BRD NEXT AT END GO HSS2.	   
           IF K1 = KALITE AND 
            D1 = DONEM AND 
            T1 > TARIH GO HS3. GO HS4.
       HS4. MOVE 0 TO BA1 BA2 BA3 BA4 BA5 BA6 BBL1 BBL2 BBL3 BBL4.
           MOVE 0 TO BBL5 BBL6 Z BBL. 
           IF TANE = 220 ADD MIKTARI TO B1   
	        MOVE STNO TO STN1 SSTN1
            COMPUTE BL1 = MIKTARI * BAREM-FIATI
            ELSE IF TANE = 230 ADD MIKTARI TO B2
            MOVE STNO TO STN1 SSTN1
            COMPUTE BL2 = MIKTARI * BAREM-FIATI
            ELSE IF TANE = 240 ADD MIKTARI TO B3 
            MOVE STNO TO STN1 SSTN1
            COMPUTE BL3 = MIKTARI * BAREM-FIATI
            ELSE IF TANE = 250 ADD MIKTARI TO B4 
            MOVE STNO TO STN1 SSTN1
            COMPUTE BL4 = MIKTARI * BAREM-FIATI
            ELSE IF TANE = 260 ADD MIKTARI TO B5 
            MOVE STNO TO STN2 SSTN2
            COMPUTE BL5 = MIKTARI * BAREM-FIATI
            ELSE IF TANE = 270 ADD MIKTARI TO B6 
            MOVE STNO TO STN2 SSTN2
            COMPUTE BL6 = MIKTARI * BAREM-FIATI
            ELSE IF TANE = 280 ADD MIKTARI TO B7 
            MOVE STNO TO STN2 SSTN2
            COMPUTE BL7 = MIKTARI * BAREM-FIATI
            ELSE IF TANE = 290 ADD MIKTARI TO B8 
            MOVE STNO TO STN2 SSTN2
            COMPUTE BL8 = MIKTARI * BAREM-FIATI
            ELSE IF TANE = 300 ADD MIKTARI TO B9 
            MOVE STNO TO STN2 SSTN2
            COMPUTE BL9 = MIKTARI * BAREM-FIATI
            ELSE IF TANE = 310 ADD MIKTARI TO B10 
            MOVE STNO TO STN3 SSTN3
            COMPUTE BL10 = MIKTARI * BAREM-FIATI
            ELSE IF TANE = 320 ADD MIKTARI TO B11 
            MOVE STNO TO STN3 SSTN3
            COMPUTE BL11 = MIKTARI * BAREM-FIATI
            ELSE IF TANE = 330 ADD MIKTARI TO B12 
            MOVE STNO TO STN3 SSTN3
            COMPUTE BL12 = MIKTARI * BAREM-FIATI
            ELSE IF TANE = 340 ADD MIKTARI TO B13 
            MOVE STNO TO STN3 SSTN3
            COMPUTE BL13 = MIKTARI * BAREM-FIATI
            ELSE IF TANE = 350 ADD MIKTARI TO B14 
            MOVE STNO TO STN3 SSTN3
            COMPUTE BL14 = MIKTARI * BAREM-FIATI
            ELSE IF TANE = 360 ADD MIKTARI TO B15 
            MOVE STNO TO STN4 SSTN4
            COMPUTE BL15 = MIKTARI * BAREM-FIATI
            ELSE IF TANE = 370 ADD MIKTARI TO B16 
            MOVE STNO TO STN4 SSTN4
            COMPUTE BL16 = MIKTARI * BAREM-FIATI
            ELSE IF TANE = 380 ADD MIKTARI TO B17 
            MOVE STNO TO STN4 SSTN4
            COMPUTE BL17 = MIKTARI * BAREM-FIATI
            ELSE IF TANE = 390 ADD MIKTARI TO B18 
            MOVE STNO TO STN4 SSTN4
            COMPUTE BL18 = MIKTARI * BAREM-FIATI
            ELSE IF TANE = 400 ADD MIKTARI TO B19 
            MOVE STNO TO STN4 SSTN4
            COMPUTE BL19 = MIKTARI *BAREM-FIATI
            ELSE IF TANE = 410 ADD MIKTARI TO B20 
            MOVE STNO TO STN5 SSTN5
            COMPUTE BL20 = MIKTARI * BAREM-FIATI
            ELSE IF TANE = 420 ADD MIKTARI TO B21 
            MOVE STNO TO STN5 SSTN5
            COMPUTE BL21 = MIKTARI * BAREM-FIATI
            ELSE IF TANE = 430 ADD MIKTARI TO B22 
            MOVE STNO TO STN5 SSTN5
            COMPUTE BL22 = MIKTARI * BAREM-FIATI
            ELSE IF TANE = 440 ADD MIKTARI TO B23  
            MOVE STNO TO STN5 SSTN5
            COMPUTE BL23 = MIKTARI * BAREM-FIATI
            ELSE IF TANE = 450 ADD MIKTARI TO B24  
            MOVE STNO TO STN5 SSTN5
            COMPUTE BL24 = MIKTARI * BAREM-FIATI
            ELSE IF TANE = 460 ADD MIKTARI TO B25 
            MOVE STNO TO STN5 SSTN5
            COMPUTE BL25 = MIKTARI * BAREM-FIATI
            ELSE IF TANE = 470 ADD MIKTARI TO B26 
            MOVE STNO TO STN6 SSTN6
            COMPUTE BL26 = MIKTARI * BAREM-FIATI
            ELSE IF TANE = 480 ADD MIKTARI TO B27 
            MOVE STNO TO STN6 SSTN6
            COMPUTE BL27 = MIKTARI * BAREM-FIATI
            ELSE IF TANE = 490 ADD MIKTARI TO B28 
            MOVE STNO TO STN6 SSTN6
            COMPUTE BL28 = MIKTARI * BAREM-FIATI
            ELSE IF TANE = 500 ADD MIKTARI TO B29 
            MOVE STNO TO STN6 SSTN6
           COMPUTE BA1 = B1 + B2 + B3 + B4.
           COMPUTE BA2 = B5 + B6 + B7 + B8 + B9.
           COMPUTE BA3 = B10 + B11 + B12 + B13 + B14. MOVE B1 TO BB1.
           COMPUTE BA4 = B15 + B16 + B17 + B18 + B19. 
           MOVE BA4 TO AB4 DAB4.
           COMPUTE BA5 = B20 + B21 + B22 + B23 + B24 + B25. 
           MOVE BA5 TO AB5 DAB5.
           COMPUTE BA6 = B26 + B27 + B28 + B29.  MOVE BA6 TO AB6 DAB6.
           COMPUTE BBL1 = BL1 + BL2 + BL3 + BL4.  MOVE B2 TO BB2.
           COMPUTE BBL2 = BL5 + BL6 + BL7 + BL8 + BL9. MOVE B3 TO BB3.
           COMPUTE BBL3 = BL10 + BL11 + BL12 + BL13 + BL14. 
           MOVE B4 TO BB4. 
		   MOVE BBL3 TO BBBL3. 
           COMPUTE BBL4 = BL15 + BL16 + BL17 + BL18 + BL19.
           MOVE B5 TO BB5.
           MOVE BBL4 TO BBBL4.
           COMPUTE BBL5 = BL20 + BL21 + BL22 + BL23 + BL24 + BL25.
           MOVE BBL5 TO BBBL5.
           COMPUTE BBL6 = BL26 + BL27 + BL28 + BL29. 
		   MOVE BBL6 TO BBBL6. 
           MOVE B6 TO BB6.
           MOVE BA1 TO AB1 DAB1. MOVE BA2 TO AB2 DAB2. 
           MOVE BA3 TO AB3 DAB3.
           COMPUTE BBL = BBL1 + BBL2 + BBL3 + BBL4 + BBL5 + BBL6. 
           MOVE B7 TO BB7. MOVE BBL1 TO BBBL1. MOVE BBL2 TO BBBL2.
           COMPUTE Z = BA1 + BA2 + BA3 + BA4 + BA5 + BA6.
           MOVE B8 TO BB8. MOVE B9 TO BB9. MOVE Z TO BOTT.
           MOVE B10 TO BB10.  MOVE B11 TO BB11.  MOVE B12 TO BB12.
           MOVE B13 TO BB13. MOVE B14 TO BB14. MOVE B15 TO BB15.
           MOVE B16 TO BB16. MOVE B17 TO BB17. MOVE B18 TO BB18.
           MOVE B19 TO BB19. MOVE B20 TO BB20. MOVE B21 TO BB21.
           MOVE B22 TO BB22. MOVE B23 TO BB23. MOVE B24 TO BB24.           
           MOVE B25 TO BB25. MOVE B26 TO BB26. MOVE B27 TO BB27.
           MOVE B28 TO BB28. MOVE B29 TO BB29.
       SSNM. GO HS3.            
       HSS2. MOVE 0 TO RRR GTORT. MOVE BOTT TO DTPKL. 
           COMPUTE GTORT = BBL / Z. MOVE BA1 TO BOT1. 
           MOVE BA2 TO BOT2. MOVE BA3 TO BOT3. MOVE BA4 TO BOT4. 
           MOVE BA5 TO BOT5. MOVE BA6 TO BOT6.
           COMPUTE RRR = Z + ZZZ.
           MOVE RRR TO ATPKL. MOVE RRR TO ZTPKL. MOVE GTORT TO TORF.
           COMPUTE YYY = BBL + BBBL. ADD BBL TO MEH. 
           MOVE MEH TO ATPBDL.
           COMPUTE GTT1 = BA1 + BBA1. COMPUTE GTT2 = BA2 + BBA2.
           COMPUTE GTT3 = BA3 + BBA3. COMPUTE GTT4 = BA4 + BBA4.
           COMPUTE GTT5 = BA5 + BBA5. COMPUTE GTT6 = BA6 + BBA6.
           COMPUTE TORF = YYY / RRR. MOVE GTT1 TO ZAB1.
           MOVE GTT2 TO ZAB2. MOVE GTT3 TO ZAB3. MOVE GTT4 TO ZAB4.
           MOVE GTT5 TO ZAB5. MOVE GTT6 TO ZAB6.
       GEWQ-D. MOVE SC TO RC.
       AQP. IF RC = 1 PERFORM E1 GO PROJEQQ. IF RC = 2 PERFORM E2 
            GO PROJEQQ.
       AQP1. IF RC = 3 PERFORM E3 GO PROJEQQ. IF RC = 4 PERFORM E4
            GO PROJEQQ.
       AQP2. IF RC = 5 PERFORM E5 GO PROJEQQ. IF RC = 6 PERFORM E6 
            GO PROJEQQ.
       E1. DISPLAY (1 1) ERASE. DISPLAY (1 1) DZ1. DISPLAY (2 1) DZ2.
           DISPLAY (3 1) DZ3. DISPLAY (4 1) DZ4. DISPLAY (5 1) DZ5.
           DISPLAY (6 1) DZ6. DISPLAY (7 1) DZ7. DISPLAY (8 1) DZ8.
           DISPLAY (9 1) DZ9. DISPLAY (17 1) DZ10. DISPLAY (18 1) DZ11.
           DISPLAY (19 1) DZ12. DISPLAY (20 1) DZ13.
           DISPLAY (21 1) DZ14. DISPLAY (22 1) DZ15.        
       E3. DISPLAY (1 1) ERASE. DISPLAY (1 1) N1. DISPLAY (2 1) N2.
           DISPLAY (3 1) N3. DISPLAY (4 1) N4. DISPLAY (5 1) N5.
           DISPLAY (8 1) N6. DISPLAY (9 1) N7. DISPLAY (10 1) N8.
           DISPLAY (11 1) N9. DISPLAY (15 1) N10. DISPLAY (16 1) N11.
           DISPLAY (17 1) N12. DISPLAY (18 1) N13.
           DISPLAY (20 1) N14. DISPLAY (22 1) N15. DISPLAY (23 1) N16.         
       E5. DISPLAY (1 1) ERASE. DISPLAY (1 1) M1. DISPLAY (2 1) M2.
           DISPLAY (3 1) M3. DISPLAY (4 1) M4. DISPLAY (5 1) M5.
           DISPLAY (6 1) M6. DISPLAY (7 1) M7. DISPLAY (8 1) M8.
           DISPLAY (9 1) M9. DISPLAY (10 1) M10. DISPLAY (11 1) M11.
           DISPLAY (12 1) M12. DISPLAY (13 1) M13. DISPLAY (14 1) M14. 
           DISPLAY (15 1) M15. DISPLAY (16 1) M16. DISPLAY (17 1) M17.
       E6. WRITE SATIR FROM M1 AFTER 24. WRITE SATIR FROM M2.
           WRITE SATIR FROM M3. WRITE SATIR FROM M4.
           WRITE SATIR FROM M5. WRITE SATIR FROM M6.
           WRITE SATIR FROM M7. WRITE SATIR FROM M8.
           WRITE SATIR FROM M9. WRITE SATIR FROM M10.
           WRITE SATIR FROM M11. WRITE SATIR FROM M12.
           WRITE SATIR FROM M13. WRITE SATIR FROM M14.
           WRITE SATIR FROM M15. WRITE SATIR FROM M16.
           WRITE SATIR FROM M17 BEFORE 25. 
       E4. WRITE SATIR FROM N1 AFTER 20. WRITE SATIR FROM N2.
           WRITE SATIR FROM N3. WRITE SATIR FROM N4.
           WRITE SATIR FROM N5. WRITE SATIR FROM N6.
           WRITE SATIR FROM N7. WRITE SATIR FROM N8.
           WRITE SATIR FROM N9. WRITE SATIR FROM N10.
           WRITE SATIR FROM N11. WRITE SATIR FROM N12.
           WRITE SATIR FROM N13. WRITE SATIR FROM N14.
           WRITE SATIR FROM N15. WRITE SATIR FROM N16 BEFORE 20.
       E2. WRITE SATIR FROM DZ1 AFTER 25. WRITE SATIR FROM DZ2.
           WRITE SATIR FROM DZ3. WRITE SATIR FROM DZ4.
           WRITE SATIR FROM DZ5. WRITE SATIR FROM DZ6.
           WRITE SATIR FROM DZ7. WRITE SATIR FROM DZ8.
           WRITE SATIR FROM DZ9. WRITE SATIR FROM DZ10.
           WRITE SATIR FROM DZ11. WRITE SATIR FROM DZ12.
           WRITE SATIR FROM DZ13. WRITE SATIR FROM DZ14.
           WRITE SATIR FROM DZ15 BEFORE 26. 
       SIFIRLAMA. MOVE 0 TO B1 B2 B3 B4 B5 B6 B7 B8 B9 B10. 
           MOVE 0 TO B11 B12 B13 B14 B15 B16 B17 B18 B19 B20 B21 B22.
           MOVE 0 TO B23 B24 B25 B26 B27 B28 B29 BL1 BL2 BL3 BL4 BL5.
           MOVE 0 TO BL6 BL7 BL8 BL9 BL10 BL11 BL12 BL13 BL14 BL15.
           MOVE 0 TO BL16 BL17 BL18 BL19 BL20 BL21 BL22 BL23 BL24 BL25.
           MOVE 0 TO BL26 BL27 BL28 BL29 Z BBL1 BBL2 BBL3 BBL4 BBL5. 
           MOVE 0 TO BBL6 BA1 BA2 BA3 BA4 BA5 BA6.
       PROJEQQ. DISPLAY (24 1) ERASE. 
           DISPLAY (24 1) "ANA MENU ICIN {ENT BAS}". 
           ACCEPT CEVAP. CLOSE BRD YAZICI. 
       SON SECTION. 
       PRKJD. DISPLAY (1 1) ERASE. STOP RUN.
