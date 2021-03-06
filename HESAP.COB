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
           DISPLAY (4 17)  "浜様様様様様様様様様様様融". 
           DISPLAY (5 17)  "麺様様様様様様様様様様様郵". 
           DISPLAY (6 17)  "�                        �". 
           DISPLAY (7 17)  "�                        �".
           DISPLAY (8 17)  "�                        �". 
           DISPLAY (9 17)  "麺様様様様様様様様様様様郵". 
           DISPLAY (10 17) "麺様曜様様僕様曜様様僕様郵". 
           DISPLAY (11 17) "� C  �  7 �  8 �  9 � �  �". 
           DISPLAY (12 17) "麺様洋様様陵様洋様様陵様郵". 
           DISPLAY (13 17) "� E  �  4 �  5 �  6 � x  �". 
           DISPLAY (14 17) "麺様洋様様陵様洋様様陵様郵". 
           DISPLAY (15 17) "� �  �  1 �  2 �  3 � -  �". 
           DISPLAY (16 17) "麺様洋様様陵様洋様様陵様郵". 
           DISPLAY (17 17) "� %  �  . �  0 � =  � +  �". 
           DISPLAY (18 17) "麺様洋様様陵様洋様様陵様郵". 
           DISPLAY (19 17) "�  � �  � �  � �  � � F  �". 
           DISPLAY (20 17) "麺様擁様様瞥様擁様様瞥様郵". 
           DISPLAY (21 17) "藩様様様様様様様様様様様夕".
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