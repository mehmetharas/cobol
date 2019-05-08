       IDENTIFICATION DIVISION.
       PROGRAM-ID. HESAP.
       AUTHOR. MEHMET HARAS.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 A1 PIC 9(13)V99 VALUE 0.
       77 A2 PIC 9(13)V99 VALUE 0.
       77 QQA3 PIC 9(13)V99 VALUE 0.
       77 A4 PIC 9(13)V99 VALUE 0.
       77 A5 PIC 9(13)V99 VALUE 0.
       77 ES PIC X(2) VALUE SPACES.
       77 ESCKOD PIC X(6) VALUE SPACES.
       SCREEN SECTION.
       01 E1.
           02 LINE 7 COLUMN 23 PIC 9(13)V99 USING A1.
       01 E2.
           02 LINE 7 COLUMN 23 PIC 9(13)V99 USING A2.
       01 E3.
           02 LINE 7 COLUMN 23 PIC 9(13)V99 USING QQA3.
       01 E4.
           02 LINE 7 COLUMN 23 PIC 9(13)V99 USING A4.
       01 E5.
           02 LINE 7 COLUMN 40 AUTO PIC X(2) USING ES.
       PROCEDURE DIVISION.
       BASLAHESAP. DISPLAY (1 1) ERASE. MOVE 0 TO A1 A2 QQA3 A4.
           DISPLAY (4 17)  "ษออออออออออออออออออออออออป". 
           DISPLAY (5 17)  "ฬออออออออออออออออออออออออน". 
           DISPLAY (6 17)  "บ                        บ". 
           DISPLAY (7 17)  "บ                        บ".
           DISPLAY (8 17)  "บ                        บ". 
           DISPLAY (9 17)  "ฬออออออออออออออออออออออออน". 
           DISPLAY (10 17) "ฬออออหออออหออออหออออหออออน". 
           DISPLAY (11 17) "บ C  บ  7 บ  8 บ  9 บ ๖  บ". 
           DISPLAY (12 17) "ฬออออฮออออฮออออฮออออฮออออน". 
           DISPLAY (13 17) "บ E  บ  4 บ  5 บ  6 บ x  บ". 
           DISPLAY (14 17) "ฬออออฮออออฮออออฮออออฮออออน". 
           DISPLAY (15 17) "บ ฎ  บ  1 บ  2 บ  3 บ -  บ". 
           DISPLAY (16 17) "ฬออออฮออออฮออออฮออออฮออออน". 
           DISPLAY (17 17) "บ %  บ  . บ  0 บ =  บ +  บ". 
           DISPLAY (18 17) "ฬออออฮออออฮออออฮออออฮออออน". 
           DISPLAY (19 17) "บ  ่ บ  ํ บ  ์ บ  ‘ บ F  บ". 
           DISPLAY (20 17) "ฬออออสออออสออออสออออสออออน". 
           DISPLAY (21 17) "ศออออออออออออออออออออออออผ".
       HESAP1. ACCEPT E1. ACCEPT E5.
       ISLEM. IF ES = "%" GO YUZDE-HES
           ELSE IF ES = "*" GO CARPIM-HES
           ELSE IF ES = "/" GO BOLUM-HES
           ELSE IF ES = "-" GO CIKARMA-HES
           ELSE IF ES = "+" GO TOPLAMA-HES
           ELSE IF ES = "=" GO SONUC-HES
           ELSE IF ES = "C" GO DEG-SIFIR
           ELSE IF ES = "E" GO HES-SIFIR
           ELSE IF ES = "F" GO SON ELSE GO HESAP1.
       YUZDE-HES. IF A2 = 0  ADD A1 TO A2  PERFORM HESAP1.
           QQA3 = (A2 * A1)/100. ADD QQA3 TO A4 A2. GO HESAP1.
       BOLUM-HES. IF A2 = 0  ADD A1 TO A2  PERFORM HESAP1.
           QQA3 = A2 / A1. ADD QQA3 TO A4 A2. GO HESAP1.
       CIKARMA-HES. IF A2 = 0  ADD A1 TO A2  PERFORM HESAP1.
           QQA3 = A2 - A1. MOVE QQA3 TO A4. GO HESAP1.
       TOPLAMA-HES. IF A2 = 0  ADD A1 TO A2  PERFORM HESAP1.
           QQA3 = A2 + A1. MOVE QQA3 TO A4. GO HESAP1.
       CARPIM-HES. IF A2 = 0  ADD A1 TO A2  PERFORM HESAP1.
           QQA3 = A2 * A1. ADD QQA3 TO A4 A2. GO HESAP1.
       SONUC-HES. DISPLAY E4. GO HESAP1.
       DEG-SIFIR. MOVE 0 TO A1 A2. GO HESAP1.
       HES-SIFIR. MOVE 0 TO A1 A2 QQA3 A4.
       SON. STOP RUN.
