
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ADRES.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ADRES ASSIGN TO DISK
           ORGANIZATION INDEXED
           ACCESS MODE DYNAMIC
           RECORD KEY ANAHTAR
           FILE STATUS DURUM.
       DATA DIVISION.
       FILE SECTION.
       FD ADRES LABEL RECORD STANDARD VALUE OF FILE-ID "ADRES.DAT".
       01 ADRES-KAYDI.
           02 ANAHTAR   PIC X(30).
           02 AD-SOYAD  PIC X(30).
           02 TEL       PIC X(20).
           02 SEHIR     PIC X(20).
       WORKING-STORAGE SECTION.
       77 SEC PIC 9.
       77 UYARI-1       PIC X(60).
       77 UYARI-2       PIC X(60).
       77 GORUNTU       PIC X(55).
       77 CEVAP         PIC X.
       77 SIFRE         PIC X(5).
       77 ILK           PIC X(30) VALUE SPACES.
       77 BIT           PIC X(30) VALUE SPACES.
       77 ANAHTAR2      PIC X(30).
       77 AD-SOYAD2     PIC X(30).
       77 TEL2          PIC X(20).
       77 SEHIR2        PIC X(20).
       01 TUSLAR PIC XX.
           88 ENTER-KEY VALUE "00".
           88 ESC-KEY   VALUE "01".
           88 F1-KEY    VALUE "02".
           88 F2-KEY    VALUE "03".
       01 DURUM PIC XX VALUE SPACES.
           88 DUR1 VALUE "30".
       SCREEN SECTION.
       01 BOS.
           02 BLANK SCREEN.
       01 SIFRE-EKRANI.
           02 LINE  8 COLUMN 23 REVERSE-VIDEO "#########################
      -    "#########".
           02 LINE  9 COLUMN 23 REVERSE-VIDEO "#         A D R E S L E R
      -    "        #".
           02 LINE 10 COLUMN 23 REVERSE-VIDEO "# ���������           ���
      -    "������  #".
           02 LINE 11 COLUMN 23 REVERSE-VIDEO "#      ��������   �������
      -    "��      #".
           02 LINE 12 COLUMN 23 REVERSE-VIDEO "#      SIFRE  =       =
      -    "        #".
           02 COLUMN 39 PIC X(5) USING SIFRE REVERSE-VIDEO AUTO SECURE.
           02 LINE 13 COLUMN 23 REVERSE-VIDEO "#
      -    "        #".
           02 LINE 14 COLUMN 23 REVERSE-VIDEO "#########################
      -    "#########".
       01 ANA-MENU.
           02 LINE 1 COLUMN 1 REVERSE-VIDEO "!!! OSB�M B�LG�SAYAR ve B�L
      -    "G� ��LEM MERKEZ�                            BANDIRMA ".
           02 LINE 2, BLANK LINE.
           02 LINE 3, BLANK LINE.
           02 COLUMN 26 HIGHLIGHT "!!! ADRES  SE��M  MEN�S� !!!".
           02 LINE 4, BLANK LINE.
           02 COLUMN 30 HIGHLIGHT "********************".
           02 LINE 5, BLANK LINE.
           02 LINE 6, BLANK LINE.
           02 COLUMN 17 HIGHLIGHT "�������������������������������������
      -    "��������ͻ".
           02 LINE 7, BLANK LINE.
           02 COLUMN 17 HIGHLIGHT "�  ����������������������������������
      -    "�����ͻ  �".
           02 LINE 8, BLANK LINE.
           02 COLUMN 17 HIGHLIGHT "�  �  �������������������������������
      -    "��ͻ  �  �".
           02 LINE 9, BLANK LINE.
           02 COLUMN 17 HIGHLIGHT "�  �  �
      -    "   �  �  �".
           02 LINE 10, BLANK LINE.
           02 COLUMN 17 HIGHLIGHT "�  �  �   1.) DOSYA OLU�TURMA
      -    "   �  �  �".
           02 LINE 11, BLANK LINE.
           02 COLUMN 17 HIGHLIGHT "�  �  �
      -    "   �  �  �".
           02 LINE 12, BLANK LINE.
           02 COLUMN 17 HIGHLIGHT "�  �  �   2.) ADRES  G�R���
      -    "   �  �  �".
           02 LINE 13, BLANK LINE.
           02 COLUMN 17 HIGHLIGHT "�  �  �
      -    "   �  �  �".
           02 LINE 14, BLANK LINE.
           02 COLUMN 17 HIGHLIGHT "�  �  �   3.) ADRES  ARAMA
      -    "   �  �  �".
           02 LINE 15, BLANK LINE.
           02 COLUMN 17 HIGHLIGHT "�  �  �
      -    "   �  �  �".
           02 LINE 16, BLANK LINE.
           02 COLUMN 17 HIGHLIGHT "�  �  �   4.) EKRANA L�STELEME
      -    "   �  �  �".
           02 LINE 17, BLANK LINE.
           02 COLUMN 17 HIGHLIGHT "�  �  �
      -    "   �  �  �".
           02 LINE 18, BLANK LINE.
           02 COLUMN 17 HIGHLIGHT "�  �  �   5.) PROGRAMDAN �IKI�
      -    "   �  �  �".
           02 LINE 19, BLANK LINE.
           02 COLUMN 17 HIGHLIGHT "�  �  �
      -    "   �  �  �".
           02 LINE 20, BLANK LINE.
           02 COLUMN 17 HIGHLIGHT "�  �  �������������������������������
      -    "��ͼ  �  �".
           02 LINE 21, BLANK LINE.
           02 COLUMN 17 HIGHLIGHT "�  ����������������������������������
      -    "�����ͼ  �".
           02 LINE 22, BLANK LINE.
           02 COLUMN 17 HIGHLIGHT "�������������������������������������
      -    "��������ͼ".
           02 LINE 23, BLANK LINE.
           02 LINE 24, BLANK LINE.
           02 COLUMN 10 HIGHLIGHT "
      -    "SE��M�N�Z NED�R ..: < >".
           02 LINE 25, BLANK LINE.





                 ���������������������������������������Ŀ
                 ��������������   M E N �   ���������������
                 �                                       ���
                 �                                       ���
                 �                                       ���
                 �                                       ���
                 �                                       ���
                 �                                       ���
                 �                                       ���
                 ����������������������������������������ٲ�
                   �����������������������������������������
                     ���������������������������������������

































       01 BASLIK.
           02 LINE 1  COLUMN 33 REVERSE-VIDEO BLINK "!! A D R E S !!".
           02 LINE 3  COLUMN 25 PIC X(30) USING GORUNTU REVERSE-VIDEO.
           02 LINE 4  COLUMN 25 PIC X(30) FROM ALL "=".
       01 SORU0.
           02 LINE 13 COLUMN 10 HIGHLIGHT "   K���N�N AD VE SOYADI .: ".
           02 LINE 13 COLUMN 38 PIC X(30) USING AD-SOYAD REVERSE-VIDEO.
       01 SORU1.
           02 LINE 15 COLUMN 10 HIGHLIGHT "   K���N�N TELEFONU .....: ".
           02 LINE 15 COLUMN 38 PIC X(20) USING TEL REVERSE-VIDEO.
           02 LINE 17 COLUMN 10 HIGHLIGHT "   �EH�R ................: ".
           02 LINE 17 COLUMN 38 PIC X(20) USING SEHIR REVERSE-VIDEO.
       01 MES0.
           02 LINE 24 COLUMN 1 HIGHLIGHT BLINK ">>>".
       01 MES1.
           02 LINE 24 COLUMN 5 REVERSE-VIDEO " [F1] ==> DEVAM       [F2]
      -    " ==> KAYIT       [ESC] ==> �IKI�                 ".
       01 MES2.
           02 LINE 24 COLUMN 5 REVERSE-VIDEO "      BU KAYIT �NCEDEN G�R
      -    "�LM��T�R L�TFEN           TU�UNA BASINIZ !!!     ".
           02 COLUMN 47 REVERSE-VIDEO BLINK "< ENTER >".
       01 MES3.
           02 LINE 24 COLUMN 5 REVERSE-VIDEO "      G�R�LEN B�LG�LER DO�
      -    "RU MU  (E/H)  ==> [ ]                            ".
           02 COLUMN 50 PIC X USING CEVAP REVERSE-VIDEO AUTO.
       01 MES4.
           02 LINE 24 COLUMN 5 REVERSE-VIDEO "      [F1] ==> DEVAM
      -    "                 [ESC] ==> �IKI�                 ".
       01 MES5.
           02 LINE 24 COLUMN 5 REVERSE-VIDEO "      ARADI�INIZ K���N�N A
      -    "D VE SOYADINI G�R�N�Z !!!                        ".
       01 MES6.
           02 LINE 24 COLUMN 5 REVERSE-VIDEO "      ARANILAN KAYIT BULUN
      -    "AMADI L�TFEN            TU�UNA BASINIZ !!!       ".
           02 COLUMN 45 REVERSE-VIDEO BLINK "< ENTER >".
       01 BAS1.
           02 LINE  6 COLUMN 1 REVERSE-VIDEO "     K���N�N AD VE SOYADI
      -    "    ".
           02 COLUMN 35 REVERSE-VIDEO "  TELEFON NUMARASI  ".
           02 COLUMN 60 REVERSE-VIDEO "�KAMET ETT��� �EH�R ".
           02 LINE  7 COLUMN 1 HIGHLIGHT    "***************************
      -    "***".
           02 COLUMN 35 HIGHLIGHT "********************".
           02 COLUMN 60 HIGHLIGHT "********************".
       01 BAS2.
           02 LINE 10 COLUMN 11 REVERSE-VIDEO "! �LK AD VE SOYADI G�R�N�
      -    "Z .:".
           02 LINE 12 COLUMN 11 REVERSE-VIDEO "! SON AD VE SOYADI G�R�N�
      -    "Z .:".
           02 LINE 20 COLUMN 23 REVERSE-VIDEO BLINK "< �LK VER�   = A >
      -    "  VE   < SON VER�   = Z >".
           02 LINE 22 COLUMN 23 REVERSE-VIDEO       " G�R�LD��� TAKD�RDE
      -    " T�M VER� D�K�M� ALINIR  ".
       01 BAS2-GIR.
           02 LINE 10 COLUMN 42 PIC X(30) USING ILK HIGHLIGHT UNDERLINE.
           02 LINE 12 COLUMN 42 PIC X(30) USING BIT HIGHLIGHT UNDERLINE.
       01 BAS3.
           02 LINE 15 COLUMN 10 HIGHLIGHT "!!!  BA�LANGI� VE B�T�� L�M�T
      -    "LER� DO�RU G�R�LD� M� (E/H) ..:".
           02 LINE 15 COLUMN 71 PIC X TO CEVAP REVERSE-VIDEO AUTO.
       01 IMDAT-EKRANI.
           02 LINE  3 COLUMN 27 REVERSE-VIDEO " !!!!!   D � K K A T   !!
      -    "!!! ".
           02 LINE  7 COLUMN  8 HIGHLIGHT "G�RM�� OLDU�UNUZ BU SE�ENEK
      -    "       DEFAYA MAHSUS SE��LMEL�D�R !!!".
           02 COLUMN 36 REVERSE-VIDEO "< B�R >".
           02 LINE  9 COLUMN  8 HIGHLIGHT "E�ER �K�NC� B�R SE��MDE DAHA
      -    "BULUNURSANIZ G�RM�� OLDU�UNUZ �NCEK�".
           02 LINE 11 COLUMN 25 REVERSE-VIDEO "VER�LER�N T�M� S�L�NECEKT
      -    "�R !!!".
           02 LINE 13 COLUMN 10 HIGHLIGHT "BU SE�ENEK VER�LER�N DEPOLANA
      -    "CA�I DOSYANIN �LK OLARAK OLU�MASI".
           02 LINE 15 COLUMN 29 HIGHLIGHT "A�AMASINDA SE��LMEL�D�R.".
           02 LINE 17 COLUMN 23 HIGHLIGHT "DOSYA VARSA BU SE�ENE�E GEREK
      -    " YOKTUR.".
           02 LINE 22 COLUMN 16 REVERSE-VIDEO "!!!!!    DOSYA OLU�SUN MU
      -    " (E/H) ===> < >    !!!!!".
           02 COLUMN 54 PIC X USING CEVAP REVERSE-VIDEO AUTO.
       01 MES7.
           02 LINE 24 COLUMN 16 REVERSE-VIDEO "!!! ��LEM  SONU  ���N [EN
      -    "TER] TU�UNA BASINIZ !!!".
       01 MES8.
           02 LINE 24 COLUMN 16 REVERSE-VIDEO "!!! L�STEN�N DEVAMI ���N
      -    "[ENTER] TU�UNA BASINIZ !!!".
       PROCEDURE DIVISION.
       ADR-AC.
           DISPLAY BOS SIFRE-EKRANI.
           ACCEPT SIFRE-EKRANI.
           IF SIFRE NOT = "O    " GO ADR-AC.
       A10. DISPLAY BOS ANA-MENU.
       A11. ACCEPT (24 68) SEC WITH AUTO-SKIP BEEP.
           GO OLUSTUR GIRIS ARAMA LISTE BITIR DEPENDING ON SEC.
           IF SEC > 6 DISPLAY (1 1) ERASE. GO A10.


       OLUSTUR.
           DISPLAY BOS IMDAT-EKRANI.
           ACCEPT IMDAT-EKRANI.
           IF CEVAP NOT = "E" GO A10
           ELSE OPEN OUTPUT ADRES CLOSE ADRES. GO A10.
       GIRIS.
           OPEN I-O ADRES.
       GIR.
           PERFORM TEMIZLE.
           MOVE "****  KAYIT G�R��� B�L�M�  ****" TO GORUNTU.
           DISPLAY (1 1) ERASE. DISPLAY SORU0 BASLIK MES0 MES1.
           ACCEPT SORU0.
           MOVE AD-SOYAD TO ANAHTAR.
           READ ADRES INVALID KEY GO GIR1.
           DISPLAY (24 1) ERASE. DISPLAY MES0 MES2.
           DISPLAY SORU0 SORU1.
           ACCEPT (24 78) TUSLAR.
           ACCEPT TUSLAR FROM ESCAPE KEY.
           IF ENTER-KEY DISPLAY (24 1) ERASE GO GIR.
       GIR1.
           DISPLAY BASLIK SORU0 SORU1 MES0 MES1.
           ACCEPT SORU1.
       AL. ACCEPT (24 78) TUSLAR.
           ACCEPT TUSLAR FROM ESCAPE KEY.
           IF F1-KEY GO GIR
           ELSE IF F2-KEY
                DISPLAY (24 1) ERASE
                DISPLAY MES0 MES3
                ACCEPT MES3
                  IF CEVAP NOT = "E" DISPLAY (24 1) ERASE
                  DISPLAY MES0 MES1 GO AL
                  ELSE WRITE ADRES-KAYDI
                     DISPLAY (24 1) ERASE
                     DISPLAY MES0 MES1
                     GO AL
           ELSE IF ESC-KEY GO SON
           ELSE GO GIR1.
       ARAMA.
           OPEN I-O ADRES.
       ARA.
           PERFORM TEMIZLE.
           MOVE "****  KAYIT ARAMA B�L�M�  ****" TO GORUNTU.
           DISPLAY (1 1) ERASE.
           DISPLAY MES0 MES5 SORU0 BASLIK.
           ACCEPT  SORU0.
           MOVE AD-SOYAD TO ANAHTAR.
           READ ADRES INVALID KEY GO YOK.
           DISPLAY (24 1) ERASE. DISPLAY MES0 MES4 SORU0 SORU1.
       AL1. ACCEPT (24 78) TUSLAR.
           ACCEPT TUSLAR FROM ESCAPE KEY.
           IF F1-KEY GO ARA
           ELSE IF ESC-KEY GO SON
           ELSE GO AL1.
       LISTE.
           DISPLAY BOS.
           MOVE "******  L�STELEME B�L�M�  ******" TO GORUNTU.
           DISPLAY BASLIK.
           OPEN I-O ADRES.
           DISPLAY BAS2.
       LIST1. MOVE ALL SPACE TO ILK BIT.
           ACCEPT BAS2-GIR.
           DISPLAY BAS3.
           ACCEPT BAS3.
           IF CEVAP = "H" GO LIST1.
           MOVE ILK TO ANAHTAR.
           DISPLAY (1 1) ERASE.
           MOVE "******  L�STELEME B�L�M�  ******" TO GORUNTU.
           DISPLAY BASLIK BAS1.
           MOVE 8 TO LIN.
           START ADRES KEY IS NOT LESS ANAHTAR INVALID KEY GO LISTE-SON.
       LST.
           READ ADRES NEXT AT END GO LISTE-SON.
           IF ANAHTAR > BIT GO LISTE-SON.
       LIST2.
           MOVE AD-SOYAD TO AD-SOYAD2.
           MOVE TEL TO TEL2.
           MOVE SEHIR TO SEHIR2.
           ADD 1 TO LIN.
           DISPLAY (LIN , 33) "�".
           DISPLAY (LIN , 57) "�".
           DISPLAY (LIN , 1)  AD-SOYAD2.
           DISPLAY (LIN , 35) TEL2.
           DISPLAY (LIN , 60) SEHIR2.
           IF LIN NOT = 21 GO LST
              ELSE DISPLAY (24 1) ERASE
                 DISPLAY MES8
                 ACCEPT CEVAP
                 DISPLAY (1 1) ERASE
                 MOVE "******  L�STELEME B�L�M�  ******" TO GORUNTU
                 DISPLAY BASLIK BAS1
                 MOVE 0 TO LIN
                 MOVE 8 TO LIN
                 GO LST.
       LISTE-SON.
           DISPLAY (24  1) ERASE.
           DISPLAY MES7.
           ACCEPT CEVAP. GO SON.
       TEMIZLE. MOVE SPACES TO AD-SOYAD TEL SEHIR ANAHTAR CEVAP.
           MOVE ZEROS TO SEC.
       YOK.
           DISPLAY (24 1) ERASE DISPLAY MES0 MES6.
           ACCEPT (24 78) TUSLAR.
           ACCEPT TUSLAR FROM ESCAPE KEY.
           IF ENTER-KEY
           DISPLAY MES0 MES4
           GO AL1
           ELSE GO YOK.
       SON.
           DISPLAY (1 1) ERASE.
           CLOSE ADRES. GO A10.
       BITIR. DISPLAY BOS. STOP RUN.
