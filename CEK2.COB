       BASLANGIC. DISPLAY (1 1) ERASE. MOVE 0 TO ILK-TARIH SON-TARIH.
           DISPLAY (8 1) "ILK TARIH <GG/AA/YY>:  /  /  ".
           DISPLAY (10 1) "SON TARIH <GG/AA/YY>:  /  /  ".
           IF KONTROL = "A" GO BB0. DISPLAY (23 1)
           "ILK TARIH = < 00/00/00 > VE SON TARIH = <99/99/99>".
           DISPLAY (24 1) "GIRERSEN TUM KAYITLARI GORUNTULERSIN".
       BB0. ACCEPT (8 22) ILK-GUN WITH PROMPT UPDATE.
           ACCEPT (8 26) ILK-AY WITH PROMPT UPDATE.
           ACCEPT (8 29) ILK-YIL WITH PROMPT UPDATE.
           DISPLAY (15 5) "ILK TARIH DOGRUMU (E/H):"
           ACCEPT (15 30) CEVAP. 
  	       IF CEVAP = "H" DISPLAY (15 5) 
		   "                            " GO BB0. 
       BB1. ACCEPT (10 22) SON-GUN WITH PROMPT UPDATE.
           ACCEPT (10 26) SON-AY WITH PROMPT UPDATE.
           ACCEPT (10 29) SON-YIL WITH PROMPT UPDATE.
           DISPLAY (15 5) "SON TARIH DOGRUMU?:".
           ACCEPT (15 30) CEVAP.
  		   IF CEVAP = "H" DISPLAY (15 5) 
		   "                              " GO BB1.
