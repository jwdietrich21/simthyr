� { Thyreostat-Simulator  	}�  { Modell 9                    	}� { ----------------------	}�
 { GRAPH						}��# A{ J. W. Dietrich, Klinikum Innenstadt der LMU M�nchen 1995 - 99 }�? y{ Prozeduren graph, zeichne_kurve, skalierung und sortieren ver�ndert und erweitert nach Hartmut Neuber, FH L�beck 1988 }��  Rgraph���  @��~SimThyrTypes����GR_STANDARD_BREITEz ���
GR_STANDARD_HOEHEz ����~factor|~i0|~i1�~longint�~
graphready�~boolean�~gr_y0|~gr_y1|~gr_y2|~gr_y3|~gr_y4|~gr_y5|~gr_y6|~gr_y7|~gr_y8|~gr_y9|~gr_y10�~real�~gr_x0|~gr_x1|~gr_x2|~gr_x3|~gr_x4|~gr_x5�~real�~gr_xmin|~gr_xmax|~	gr_xdelta|~gr_ymin|~gr_ymax|~	gr_ydelta�~real�~gr_w_zeilen|~gr_i|~	gr_y_wert|~	gr_x_wert|~gr_y_skala1�~integer�~gr_y_skala2|~
gr_x_skala|~gr_s|~gr_b|~gr_kriterium�~integer�~
gr_puffer1|~
gr_puffer2�~integer�~gr_xe|~gr_ye�~integer�~gr_max_breite|~gr_max_hoehe�~integer�~	gr_weiter�p  �~gr_antw�p  �~gr_x_schrift�p  �~gr_y_schrift�p  	�~gr_titel�p  O�~	gr_nummer|~antwort|~	antwort_p�p  �~
gr_schluss�p  �~gr_text�~str255��~	gLaufbild�~	PicHandle�~	gGraphikFenster�~	WindowPtr���  nstunden�~x�~real��~str255��  �	ermitteln��  �linie�~gr_x1|~gr_y1|~gr_x2|~gr_y2�~integer���  �zeichne_rahmen��  �	sortieren��	  
skalierung��
  Fschreibe_text��~tx|~ty�~integer�~text�r���
  zzeichne_kurve�~
vorzeichen�~integer���  �Graph���  ���  (	ermitteln��# A		{ermittelt x- und y-Koordinaten im Verh�ltnis zur Fenstergr��e}�~Breite|~Hoehe�~integer�~Vx|~Vy�~real��  "  �\gGraphikFensterTR\portRect�      �x\Breite
\right>\left�x\Hoehe
\bottom>\top���x\Vx
\Breite2\gr_max_breite�x\Vy
\Hoehe2\gr_max_hoehe�x\gr_xe
\round\Vx0\gr_xe�x\gr_ye
\round\Vy0\gr_ye�����  �stunden�~x�~real��~Str255�� /		{Rechnet Sekunden-Werte in hms-Dastellung um}�~d|~h|~m|~s|~r�~longint��  

  
x	\r
\trunc\x�x\d
\r4Z Q��x\r
\r6Z Q��x\h
\r4Z�x\r
\r6Z�x\m
\r4Z <�x\r
\r6Z <�x\s
\r��\dDZ  x,\stunden
\StringOfZ    Z \hZ Z:Z \mZ Z:Z \sZ �x0\stunden
\StringOf\dZ Zd Z \hZ Z:Z \mZ Z:Z \sZ �����  
Nlinie�~gr_x1|~gr_y1|~gr_x2|~gr_y2�~integer���  
�  
�x\MoveTo\gr_x1\gr_y1�x\LineTo\gr_x2\gr_y2�����  
�zeichne_rahmen��  �  �x\gr_xe
\GR_STANDARD_BREITE>Z �x\gr_ye
\GR_STANDARD_HOEHE>Z �x\gr_max_hoehe
\GR_STANDARD_HOEHE>Z �~	ermitteln��  7													{Umrechnung auf tats�chliche Fenstergr��e}x\gr_xe
\gr_xe>Z ��  &									{ber�cksichtigt Scrollbalken}x\gr_ye
\gr_ye>Z �x\linieZ Z \gr_xeZ �x\linieZ \gr_ye\gr_xe\gr_ye�x\linieZ Z Z \gr_ye�x\linie\gr_xeZ \gr_xe\gr_ye�x\gr_max_hoehe
\GR_STANDARD_HOEHE�����   	sortieren�� 1           {Bestimmung der Maximal/Minimal-Werte}�~m�~integer��  �  �x\gr_xmax
\WerteZ Z <\i0�x\gr_xmin
\WerteZ Z <\i0�x\gr_ymax
\WerteZ Z <\i0�x\gr_ymin
\WerteZ Z <\i0��
mZ <\i0\nmax�      ؒ     {Sortieren der Werte}�\WerteZ \mJ\gr_xmaxx\gr_xmax
\WerteZ \m��\WerteZ \mH\gr_xminx\gr_xmin
\WerteZ \m��\WerteZ \mJ\gr_ymaxx\gr_ymax
\WerteZ \m��\WerteZ \mH\gr_yminx\gr_ymin
\WerteZ \m�������	  
skalierung��~i�~integer��  �  �x\gr_xe
Z Z��  				{Skalierung der x-Achse}~	ermitteln�x\
gr_x_skala
\gr_xe��iZ Z �      Xx\gr_ye
ZX�~	ermitteln�x\
gr_puffer2
\gr_ye�x\gr_ye
Z\�~	ermitteln�x\linie\
gr_x_skala\
gr_puffer2\
gr_x_skala\gr_ye�x\gr_xe
Z �~	ermitteln�x\
gr_x_skala
\
gr_x_skala<\gr_xe���x\gr_ye
Z ��  				{Skalierung der y-Achse}~	ermitteln�x\gr_y_skala1
\gr_ye��iZ Z �      �x\gr_xe
Z V�~	ermitteln�x\
gr_puffer1
\gr_xe�x\gr_xe
Z Z�~	ermitteln�x\linie\
gr_puffer1\gr_y_skala1\gr_xe\gr_y_skala1�x\gr_ye
Z  �~	ermitteln�x\gr_y_skala1
\gr_y_skala1<\gr_ye���x\gr_ye
Z (�~	ermitteln�x\gr_y_skala2
\gr_ye��iZ Z 
�      �x\gr_xe
Z V�~	ermitteln�x\
gr_puffer1
\gr_xe�x\gr_xe
Z Z�~	ermitteln�x\linie\
gr_puffer1\gr_y_skala2\gr_xe\gr_y_skala2�x\gr_ye
Z  �~	ermitteln�x\gr_y_skala2
\gr_y_skala2<\gr_ye�������
  schreibe_text��~tx|~ty�~integer�~text�r���  �  �x\gr_xe
\tx�x\gr_ye
\ty�~	ermitteln�x\MoveTo\gr_xe\gr_ye�x\
DrawString\text�����
  �zeichne_kurve�~
vorzeichen�~integer���~gr_xpkt1|~gr_ypkt1|~gr_xpkt2|~gr_ypkt2|~m�~integer�~gr_faktor_x|~gr_faktor_y|~gr_x|~gr_y|~gr_dx|~gr_dy�~real�~
gr_start_x|~
gr_start_y|~gr_d_xraster|~gr_d_yraster�~integer�~
lok_puffer�~integer��  �  �x\gr_xe
Z Z�~	ermitteln�x\
gr_start_x
\gr_xe��
\
vorzeichenDZ �      �x\gr_ye
Z��~	ermitteln�x\
lok_puffer
\gr_ye�x\gr_ye
ZX�~	ermitteln�x\
gr_start_y
\
lok_puffer>\gr_ye����
\
vorzeichenDZ �      Fx\gr_ye
Z��~	ermitteln�x\
lok_puffer
\gr_ye�x\gr_ye
Z ��~	ermitteln�x\
gr_start_y
\
lok_puffer>\gr_ye����
\
vorzeichenDZ �      �x\gr_ye
Z��~	ermitteln�x\
lok_puffer
\gr_ye�x\gr_ye
Z �~	ermitteln�x\
gr_start_y
\
lok_puffer>\gr_ye���x\gr_xe
Z d�x\gr_ye
Z  �~	ermitteln�x\gr_d_xraster
\gr_xe�x\gr_d_yraster
\gr_ye�x\gr_dx
\gr_x2>\gr_x1�x\gr_faktor_x
\gr_d_xraster2\gr_dx�x\gr_dy
\gr_y2>\gr_y1�x\gr_faktor_y
\gr_d_yraster2\gr_dy��x\gr_x
\WerteZ Z <\i0�x\gr_y
\WerteZ Z <\i0��\
vorzeichenDZ 8\gr_yHZ  x\gr_y
Z  ��\gr_xHZ  x\gr_x
Z  �x\gr_xpkt1
\round\gr_x0\gr_faktor_x<\
gr_start_x�x\gr_ypkt1
\round\gr_y0\gr_faktor_y<\
gr_start_y�x\gr_ye
Z��~	ermitteln�x\gr_ypkt1
\gr_ye>\gr_ypkt1���m\i0<Z \nmax�      �x\gr_x
\WerteZ \m�x\gr_y
\WerteZ \m��\
vorzeichenDZ 8\gr_yHZ  x\gr_y
Z  ��\gr_xHZ  x\gr_x
Z  �x\gr_xpkt2
\round\gr_x0\gr_faktor_x<\
gr_start_x�x\gr_ypkt2
\round\gr_y0\gr_faktor_y<\
gr_start_y�x\gr_ye
Z��~	ermitteln�x\gr_ypkt2
\gr_ye>\gr_ypkt2�x\linie\gr_xpkt1\gr_ypkt1\gr_xpkt2\gr_ypkt2�x\gr_xpkt1
\gr_xpkt2�x\gr_ypkt1
\gr_ypkt2�������  bGraph��~reply�~integer�~oldPort�~GrafPtr�~origFont|~origSize�~integer��      .�   {Graph-Hauptteil}�\
graphreadyD\FALSE�      ^�x\gr_max_breite
\GR_STANDARD_BREITE�x\gr_max_hoehe
\GR_STANDARD_HOEHE�~	sortieren�x\gr_kriterium
Z ��\gr_yminNZ  8\gr_ymaxJZ  x\gr_kriterium
Z ��\gr_ymaxJZ  8\gr_yminHZ  x\gr_kriterium
Z ��\gr_yminHZ  8\gr_ymaxLZ  x\gr_kriterium
Z ��\gr_kriteriumDZ �      6x\reply
\CautionAlert\RES_BASE<Z X�x\antwort
Zn���x\
ShowWindow\gGraphikFenster���x\GetPort\oldPort�x\SetPort\gGraphikFenster�x\	gLaufbild
\OpenPicture\gGraphikFensterTR\portRect�x\origFont
\gGraphikFensterTR\txFont�x\origSize
\gGraphikFensterTR\txSize�x\TextFont\applFont�x
\TextSizeZ 	�~	Zeichne_Rahmen�� %		{Berechnung der x-Skalierungswerte}x\	gr_xdelta
\gr_xmax2Z �x\gr_x0
Z  �x\gr_x1
\gr_x0<\	gr_xdelta�x\gr_x2
\gr_x1<\	gr_xdelta�x\gr_x3
\gr_x2<\	gr_xdelta�x\gr_x4
\gr_x3<\	gr_xdelta�x\gr_x5
\gr_x4<\	gr_xdelta�� %		{Berechnung der y-Skalierungswerte}�\gr_kriteriumDZ �       �x\	gr_ydelta
\gr_ymax2Z �x
\gr_y10
\gr_ymax�x\gr_y9
\gr_y10>\	gr_ydelta�x\gr_y8
\gr_y9>\	gr_ydelta�x\gr_y7
\gr_y8>\	gr_ydelta�x\gr_y6
\gr_y7>\	gr_ydelta�x\gr_y5
Z  �x	\gr_y4
^\gr_y6�x	\gr_y3
^\gr_y7�x	\gr_y2
^\gr_y8�x	\gr_y1
^\gr_y9�x
\gr_y0
^\gr_ymax����\gr_kriteriumDZ �      "bx\	gr_ydelta
\gr_ymax2Z 
�x
\gr_y10
\gr_ymax�x\gr_y9
\gr_y10>\	gr_ydelta�x\gr_y8
\gr_y9>\	gr_ydelta�x\gr_y7
\gr_y8>\	gr_ydelta�x\gr_y6
\gr_y7>\	gr_ydelta�x\gr_y5
\gr_y6>\	gr_ydelta�x\gr_y4
\gr_y5>\	gr_ydelta�x\gr_y3
\gr_y4>\	gr_ydelta�x\gr_y2
\gr_y3>\	gr_ydelta�x\gr_y1
\gr_y2>\	gr_ydelta�x\gr_y0
Z  ����\gr_kriteriumDZ �      #�x\	gr_ydelta
\abs\gr_ymin2Z 
�x\gr_y1
\gr_y0<\	gr_ydelta�x\gr_y2
\gr_y1<\	gr_ydelta�x\gr_y3
\gr_y2<\	gr_ydelta�x\gr_y4
\gr_y3<\	gr_ydelta�x\gr_y5
\gr_y4<\	gr_ydelta�x\gr_y6
\gr_y5<\	gr_ydelta�x\gr_y7
\gr_y6<\	gr_ydelta�x\gr_y8
\gr_y7<\	gr_ydelta�x\gr_y9
\gr_y8<\	gr_ydelta�x\gr_y10
Z  �����
 {Beschriftung}x\gr_xe
Z �x\gr_ye
Z �x\schreibe_text\gr_xe\gr_ye\gr_y_schrift�x\gr_xe
ZS�x\gr_ye
ZT�x\schreibe_text\gr_xe\gr_ye\gr_x_schrift�x\gr_xe
Z #�x\gr_ye
Z #�x\gr_text
\StringOf\gr_y9�x\schreibe_text\gr_xe\gr_ye\gr_text�x\gr_xe
Z #�x\gr_ye
Z F�x\gr_text
\StringOf\gr_y8�x\schreibe_text\gr_xe\gr_ye\gr_text�x\gr_xe
Z #�x\gr_ye
Z i�x\gr_text
\StringOf\gr_y7�x\schreibe_text\gr_xe\gr_ye\gr_text�x\gr_xe
Z #�x\gr_ye
Z ��x\gr_text
\StringOf\gr_y6�x\schreibe_text\gr_xe\gr_ye\gr_text�x\gr_xe
Z #�x\gr_ye
Z ��x\gr_text
\StringOf\gr_y5�x\schreibe_text\gr_xe\gr_ye\gr_text�x\gr_xe
Z #�x\gr_ye
Z Ҙx\gr_text
\StringOf\gr_y4�x\schreibe_text\gr_xe\gr_ye\gr_text�x\gr_xe
Z #�x\gr_ye
Z ��x\gr_text
\StringOf\gr_y3�x\schreibe_text\gr_xe\gr_ye\gr_text�x\gr_xe
Z #�x\gr_ye
Z�x\gr_text
\StringOf\gr_y2�x\schreibe_text\gr_xe\gr_ye\gr_text�x\gr_xe
Z #�x\gr_ye
Z9�x\gr_text
\StringOf\gr_y1�x\schreibe_text\gr_xe\gr_ye\gr_text�x\gr_xe
Z #�x\gr_ye
Z\�x\gr_text
\StringOf\gr_y0�x\schreibe_text\gr_xe\gr_ye\gr_text�x\gr_xe
Z �x\gr_ye
Zt�xT\gr_text
\StringOf\Stunden\gr_x0Z \Stunden\gr_x1Z \Stunden\gr_x2Z \Stunden\gr_x3Z \Stunden\gr_x4Z \Stunden\gr_x5Z �x\schreibe_text\gr_xe\gr_ye\gr_text���� {zeichne_rahmen;}� 		{Bildrahmen au�en}�� 	{	Bildrahmen innen:	}x\gr_xe
Z Z�~	ermitteln�x\
gr_puffer1
\gr_xe�x\gr_xe
ZN�x\gr_ye
Z 
�~	ermitteln�x\linie\
gr_puffer1\gr_ye\gr_xe\gr_ye��x\gr_xe
Z Z�~	ermitteln�x\
gr_puffer1
\gr_xe�x\gr_xe
ZN�x\gr_ye
ZX�~	ermitteln�x\linie\
gr_puffer1\gr_ye\gr_xe\gr_ye��x\gr_ye
Z 
�~	ermitteln�x\
gr_puffer2
\gr_ye�x\gr_xe
Z Z�x\gr_ye
Z^�~	ermitteln�x\linie\gr_xe\
gr_puffer2\gr_xe\gr_ye��x\gr_ye
Z 
�~	ermitteln�x\
gr_puffer2
\gr_ye�x\gr_xe
ZN�x\gr_ye
Z^�~	ermitteln�x\linie\gr_xe\
gr_puffer2\gr_xe\gr_ye��~
skalierung���\gr_kriteriumDZ �      -tx\gr_xe
Z Z�~	ermitteln�x\
gr_puffer1
\gr_xe�x\gr_xe
ZN�x\gr_ye
Z ��~	ermitteln�x\linie\
gr_puffer1\gr_ye\gr_xe\gr_ye���x\zeichne_kurve\gr_kriterium�~ClosePicture�x\DrawPicture\	gLaufbild\gGraphikFensterTR\portRect�x\TextFont\origFont�x\TextSize\origSize�x\SetPort\oldPort���� 