echo OFF
cls
echo            �������������������������������������������
echo            �   OSBIM     DOSYA KURTARMA iSLEMi       �
echo            �������������������������������������������
IF .==%1.       GOTO yok
IF NOT EXIST %1 GOTO yok
IF EXIST E%1.*  GOTO eski
echo.
echo       (Dosyalarinizi yedeklemis olmaniz gerekir)  L�tfen bekleyiniz ...
REBUILD %1,Y%1;
REN %1.* E%1.*
REN Y%1.* %1.*
echo            �����������������������������������������������������������
echo            �           KURTARMA iSLEMi BiTTi                         �
echo            �Dosyalarinizin bozuk hali E*.* ismiyle DiSKinizde mevcut,�
echo            �          Dilerseniz (DEL E*.*) komutuyla silebilirsiniz.�
echo            �����������������������������������������������������������
GOTO cikis
:eski
echo            �������������������������������������������
echo            �  DOSYALARINIZIN ESKi HALi DiSKTE VAR,   �
echo            �  KURTARMA iSLEMi YAPILAMAZ !            �
echo            �  Dilerseniz DEL E*.* komutuyla silerek; �
echo            �       kurtarma islemini tekrarlayiniz.  �
echo            �������������������������������������������    
GOTO cikis
:yok
echo            �������������������������������������������
echo            � KURTARILACAK DOSYA DiSKTE BULUNAMADI !! �
echo            �������������������������������������������
:cikis
