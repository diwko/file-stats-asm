dane1 segment
    bufor         db    4096 dup(?) 
                  db    4096 dup(?)
                  db    4096 dup(?) 
                  db    4096 dup(?)
                  db    4096 dup(?) 
                  db    4096 dup(?)
                  db    4096 dup(?) 
                  db    4096 dup(?)
    
    
    plik_odczyt   db    20,0,22 dup(0)
    plik_zapis    db    20,0,22 dup(0)
    uchwyt_odczyt dw    0
    uchwyt_zapis  dw    0
    l_binarna     db    32 dup(0)
         
    napis_plik_odczyt db    "Podaj adres pliku do odczytu: $"
    napis_plik_zapis  db    "Podaj adres pliku do zapisu: $"  
    napis_newline     db    10,13,'$'
    napis_ostrzezenie db    "Program nadpisze plik!!! Czy kontynuowac? [t/n]$"
    napis_zly_arg     db    "Niepoprawne dane!",10,13,'$'
    napis_pocz        db    "Program zapisuje do pliku (zapis) raport odnosnie liczby poszczegolnych bajtow  danego pliku (odczyt)",10,13,'$'
    
    
    potegi2       db    0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,1,6,0,0,0,0,0,0,0,0,3,2,0,0,0,0,0,0,0,0,6,4,0,0,0,0,0,0,0,1,2,8,0,0,0,0,0,0,0,2,5,6,0,0,0,0,0,0,0,5,1,2,0,0,0,0,0,0,1,0,2,4,0,0,0,0,0,0,2,0,4,8,0,0,0,0,0,0,4,0,9,6,0,0,0,0,0,0,8,1,9,2,0,0,0,0,0,1,6,3,8,4,0,0,0,0,0,3,2,7,6,8,0,0,0,0,0,6,5,5,3,6,0,0,0,0,1,3,1,0,7,2,0,0,0,0,2,6,2,1,4,4,0,0,0,0,5,2,4,2,8,8,0,0,0,1,0,4,8,5,7,6,0,0,0,2,0,9,7,1,5,2,0,0,0,4,1,9,4,3,0,4,0,0,0,8,3,8,8,6,0,8,0,0,1,6,7,7,7,2,1,6,0,0,3,3,5,5,4,4,3,2,0,0,6,7,1,0,8,8,6,4,0,1,3,4,2,1,7,7,2,8,0,2,6,8,4,3,5,4,5,6,0,5,3,6,8,7,0,9,1,2,1,0,7,3,7,4,1,8,2,4,2,1,4,7,4,8,3,6,4,8,4,2,9,4,9,6,7,2,9,60,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,1,6,0,0,0,0,0,0,0,0,3,2,0,0,0,0,0,0,0,0,6,4,0,0,0,0,0,0,0,1,2,8
                  db    0,0,0,0,0,0,0,2,5,6,0,0,0,0,0,0,0,5,1,2,0,0,0,0,0,0,1,0,2,4,0,0,0,0,0,0,2,0,4,8,0,0,0,0,0,0,4,0,9,6,0,0,0,0,0,0,8,1,9,2,0,0,0,0,0,1,6,3,8,4,0,0,0,0,0,3,2,7,6,8,0,0,0,0,0,6,5,5,3,6,0,0,0,0,1,3,1,0,7,2,0,0,0,0,2,6,2,1,4,4,0,0,0,0,5,2,4,2,8,8,0,0,0,1,0,4,8,5,7,6,0,0,0,2,0,9,7,1,5,2,0,0,0,4,1,9,4,3,0,4,0,0,0,8,3,8,8,6,0,8,0,0,1,6,7,7,7,2,1,6,0,0,3,3,5,5,4,4,3,2,0,0,6,7,1,0,8,8,6,4,0,1,3,4,2,1,7,7,2,8,0,2,6,8,4,3,5,4,5,6,0,5,3,6,8,7,0,9,1,2,1,0,7,3,7,4,1,8,2,4,2,1,4,7,4,8,3,6,4,8,4,2,9,4,9,6,7,2,9,6
               
    
    
    dane_plik     db    "Raport z pliku: "
    n_pliku       db    21 dup(0),13,10,13,10
                  db    "N    Liczba N-tych bajtow",13,10
    tablica       db    4352 dup(48)
                 
dane1 ends

dane2 segment
      bajty       dw    512 dup(0)
dane2 ends
    


code1 segment                              

start:          			 		    
                mov		ax,seg w_stosu					;inicjalizacja stosu
				mov		ss,ax
				mov     sp,offset w_stosu 				
						
				call    wczytaj_argument
						             
		                mov     ax,seg plik_odczyt     	;ustawienie segmentu danych
				mov     ds,ax
						
				call    policz_bajty
						
				call    wypelnij_tablica
				call    zamien_bajty_na_ascii 
						
						
				mov     bx,uchwyt_odczyt
				call    zamknij_plik 
						
				call    zapisz_do_pliku				
						
				mov     bx,uchwyt_zapis
				call    zamknij_plik 
			            
		                
				mov		ax,04c00h  		    			;koniec programu i powrot do DOS
				int		21h  
				
				

;..............................

clear_screen:                                   		;czysci ekran
                push    ax
                mov     ah,00h                   
                mov     al,03                   
                int     10h
                pop     ax
                ret
;..............................
;in dx                                          		wypisuje tekst z dx zakonczony '$'

wypisz_string:
                push    ax
                mov     ah,9
                int     21h
                pop     ax
                ret    
;..............................

wczytaj_string:
                push    ax                
                mov     ah,0ah
                int     21h
                pop     ax
                ret
                
;..............................
;sprawdza argument wywolania programu

wczytaj_argument:
                push    ax
                push    cx
                push    dx
                push    di
                push    si
               
                mov     ax,seg plik_odczyt              ;ds na poczatku programu ustawioiny tam gdzie jest argument
				mov     es,ax
				mov     si,082h
				
				mov     di,offset plik_odczyt
				xor     cx,cx
				mov     cl,byte ptr ds:[080h]   				;cx-ilosc znakow
										
				cmp     cx,0
				je      brak_arg				 
			
	    pierwszy_arg:    
                mov     al,byte ptr ds:[si]
                cmp     al,' '
                je      drugi_arg_1
                mov     byte ptr es:[2 + di],al
                inc     si
                inc     di
                loop    pierwszy_arg
                
            drugi_arg_1:
                dec     cx
                cmp     cx,0
                jle     niepoprawne_argumenty
                inc     si
                mov     di,offset [plik_zapis]
            
            drugi_arg:
                mov     al,byte ptr ds:[si]
                cmp     al,' '
                je      niepoprawne_argumenty
                mov     byte ptr es:[2 + di],al
                inc     si
                inc     di
                loop    drugi_arg
                
                mov     dx,seg plik_odczyt
                mov     ds,dx
                
                call    sprawdz_argumenty        
                jmp     argumenty_wczytane
                         
           niepoprawne_argumenty:
                mov     dx,seg plik_odczyt
                mov     ds,dx
                
                mov     dx,offset [napis_zly_arg]
                call    wypisz_string
                call    wczytaj_argumenty_w_prog
                jmp     argumenty_wczytane
                
           brak_arg:
                mov     dx,seg plik_odczyt
                mov     ds,dx
                mov     dx,offset [napis_pocz]
                call    wypisz_string     
                call    wczytaj_argumenty_w_prog 
           
           argumenty_wczytane:     
                pop     si
                pop     di
                pop     dx
                pop     cx
                pop     ax
                ret
;..............................                

sprawdz_plik_o:  
                push    ax
                push    dx
            
            sprawdz_argument0:     
                call    usun_enter                
            
                xor     ax,ax                       ;al - typ otwarcia pliku  0-odczyt
                mov     dx,offset [plik_odczyt+2]   ;dx - adres pliku
                mov     ah,03dh                     ;otwieranie pliku
                int     21h
                jc      niepoprawny_argument1       ;skok jesli nie udalo sie otworzyc pliku
                                                    ;flaga cf=0 otworzono, cf=1 blad
                mov     [uchwyt_odczyt],ax          ;ax - uchwyt do pliku
                
                pop     dx
                pop     ax
                ret
            
            niepoprawny_argument1:
                mov     dx,offset [napis_zly_arg]
                call    wypisz_string
                call    wczytaj_plik_o
                jmp     sprawdz_argument0
;................................  

sprawdz_plik_z: 
                push    ax
                push    cx
                push    dx
                
            sprawdz_argument00:     
                call    usun_enter
                
                mov     ax,1                        ;al - typ otwarcia pliku    1-zapis
                mov     dx,offset [plik_zapis+2]    ;dx - adres pliku
                mov     ah,03dh                     ;otwieranie pliku
                int     21h
                jc      nowy_plik                   ;skok jesli nie udalo sie otworzyc pliku
                mov     word ptr [uchwyt_zapis],ax
                
                mov     dx,offset [napis_ostrzezenie]
                call    wypisz_string
                
            czekaj_klawisz:       
                mov     ah,8                      	;czekaj na klawisz
                int     21h                       	;n
                cmp     al,110
                je      niepoprawny_argument2
                cmp     al,116                    	;t
                je      tak_koniec    
                jmp     czekaj_klawisz
          
          nowy_plik:
                mov     dx,offset [napis_newline]       
                call    wypisz_string
                
                mov     cl,0                        ;cl - atrybuty nowego pliku                               
                mov     dx,offset [plik_zapis+2]
                mov     ah,03ch                     ;utworz nowy plik
                int     21h
                  
                jc      niepoprawny_argument2         
                
                mov     word ptr [uchwyt_zapis],ax
                pop     dx
                pop     cx
                pop     ax
                ret                
                
            niepoprawny_argument2:
                mov     dx,offset [napis_zly_arg]
                call    wypisz_string
                call    wczytaj_plik_z
                jmp     sprawdz_argument00
            
            tak_koniec:
                mov     dx,offset [napis_newline]       
                call    wypisz_string
                pop     dx
                pop     cx
                pop     ax
                ret
;..............................
            
wczytaj_plik_o: 
                push    dx

                mov     dx,offset [napis_newline]       
                call    wypisz_string
                
                mov     dx,offset [napis_plik_odczyt]
                call    wypisz_string
                mov     dx,offset [plik_odczyt]
                call    wczytaj_string 
                
                mov     dx,offset [napis_newline]
                call    wypisz_string 
                                
                pop     dx
                ret
;.............................

wczytaj_plik_z:
                push    dx

                mov     dx,offset [napis_newline]       
                call    wypisz_string
                
                mov     dx,offset [napis_plik_zapis]
                call    wypisz_string
                mov     dx,offset [plik_zapis]
                call    wczytaj_string 
                
                mov     dx,offset [napis_newline]
                call    wypisz_string 
                                
                pop     dx
                ret
;..............................

wczytaj_argumenty_w_prog:
                call    wczytaj_plik_o
                call    sprawdz_plik_o
                call    wczytaj_plik_z
                call    sprawdz_plik_z               
                ret
;..............................

sprawdz_argumenty:
                call    sprawdz_plik_o
                call    sprawdz_plik_z 
                ret
;..............................

usun_enter:                      ;usuwa znak enter z nazw pliku
                push    ax
                push    bx
                push    cx
                push    si
                
                xor     si,si 
                mov     bx,offset [plik_odczyt+2]           
            
            usun_z_odczyt:  
                mov     al,byte ptr [bx+si]
                cmp     al,13
                je      wstaw0_1
                cmp     al,0
                je      usunieto1
                inc     si
                jmp     usun_z_odczyt
                
            wstaw0_1:
                xor     cx,cx
                mov     byte ptr [bx+si],cl 
                
            usunieto1:    
                xor     si,si 
                mov     bx,offset [plik_zapis+2]
            
            usun_z_zapis:    
                mov     al,byte ptr [bx+si]
                cmp     al,13
                je      wstaw0_2
                cmp     al,0
                je      usunieto
                inc     si
                jmp     usun_z_zapis
                
            wstaw0_2:
                xor     cx,cx
                mov     byte ptr [bx+si],cl
            
            usunieto:
                pop     si
                pop     cx
                pop     bx
                pop     ax
                ret 

;..............................

policz_bajty:    
                push    ax
                push    bx
                push    cx
                push    dx                
                
                mov     dx,seg bajty
                mov     es,dx
                
                
                mov     dx,offset bufor     			;gdzie zapisac  
                mov     di,word ptr [uchwyt_odczyt]
                mov     cx,32768            			;ile znakow
                            
            wczytaj_do_buf:                
                mov     bx,di
                mov     ah,03fh             			;czytaj plik
                int     21h 
                
                mov     si,32768
                
                cmp     ax,32768
                jb      ostatni_raz1                
                
            licz_kolejne:
                
                mov     ax,word ptr [si-2]
                
                xor     bx,bx
                mov     bl,al                
                shl     bx,2
                add     word ptr es:[bx+2],1               
                adc     word ptr es:[bx],0 
              
                xor     bx,bx
                mov     bl,ah                
                shl     bx,2
                add     word ptr es:[bx+2],1
                adc     word ptr es:[bx],0              
                
                sub     si,2
                jnz     licz_kolejne
                
                jmp     wczytaj_do_buf
                
            ostatni_raz1:                
                mov     si,ax               		;sprawdzanie parzystosci
                shr     ax,1
                jnc     ostatni_raz         		;sprawdza CarryFlag
                
                xor     bx,bx
                mov     bl,byte ptr [si-2]
                shl     bx,2
                add     word ptr es:[bx+2],1               
                adc     word ptr es:[bx],0
                dec     si                 
            
            ostatni_raz:               
                mov     ax,word ptr [si-2]
                
                xor     bx,bx
                mov     bl,al                
                shl     bx,2
                add     word ptr es:[bx+2],1               
                adc     word ptr es:[bx],0 
              
                xor     bx,bx
                mov     bl,ah                
                shl     bx,2
                add     word ptr es:[bx+2],1
                adc     word ptr es:[bx],0              
                
                sub     si,2
                jnz     ostatni_raz  
                
                pop     dx
                pop     cx
                pop     bx
                pop     ax
                ret 
;..............................

wypelnij_tablica:                              ;wypelnia dane ktore zostana zapisane do pliku
                push    ax
                push    bx
                push    cx
                push    dx
                push    si
                
                mov     cx,21
                xor     si,si
                
            nazwa_pliku:       
                mov     al,byte ptr [plik_odczyt+si+2]
                mov     byte ptr [n_pliku+si],al
                inc     si
                loop    nazwa_pliku
                
                mov     bx,offset tablica
               
                mov     cx,100
                mov     si,1700 
            setki:
                mov     byte ptr [bx+si],49
                add     si,17   
                loop    setki 
                
                mov     cx,56
                mov     si,3400
            dwusetki:
                mov     byte ptr [bx+si],50
                add     si,17   
                loop    dwusetki
                
                mov     cx,256
                mov     si,1
                mov     ax,48
                xor     dx,dx
           dziesiatki:
                mov     byte ptr [bx+si],al
                add     si,17
                inc     dx
                cmp     dx,10
                jb      nie_zeruj
                inc     ax
                xor     dx,dx
                cmp     ax,58
                jb      nie_zeruj
                mov     ax,48
           nie_zeruj:
                loop    dziesiatki
                
                mov     cx,256
                mov     si,2
                mov     ax,48
           jednosci:
                mov     byte ptr [bx+si],al
                add     si,17
                inc     ax
                cmp     ax,58
                jb      nie_zeruj1
                mov     ax,48
           nie_zeruj1:
                loop    jednosci
                
                mov     cx,256
                xor     si,si
           reszta:
                mov     al,58
                mov     byte ptr [bx+si+3],al  
                mov     al,32
                mov     byte ptr [bx+si+4],al    
                mov     al,13
                mov     byte ptr [bx+si+15],al
                mov     al,10
                mov     byte ptr [bx+si+16],al
                add     si,17 
                loop    reszta
                
                pop     si
                pop     dx
                pop     cx
                pop     bx
                pop     ax
                ret    
;..............................  

;in: si - ktora liczba 
;out  - [l_binarna]
int16x16_to_bin:                                   ;zamienia liczbe zawarta w dwoch slowach na binarna
                push    ax
                push    bx
                push    dx
                push    di
                push    si
                
                mov     bx,2
                mov     di,31
                
                mov     ax,si
                mov     dx,4
                mul     dx
                mov     si,ax
                
                mov     ax,word ptr es:[si+2]
                
           dziel2v1:     
                xor     dx,dx
                div     bx
                mov     byte ptr [l_binarna+di],dl
                dec     di
                cmp     ax,0
                jne     dziel2v1
                
                mov     ax,word ptr es:[si] 
                
                cmp     di,15
                je      dziel2v2      
          
          
          uzupelnij_zera1:
                mov     byte ptr [l_binarna+di],0
                dec     di
                cmp     di,15
                jg      uzupelnij_zera1
                 
           dziel2v2:     
                xor     dx,dx
                div     bx
                mov     byte ptr [l_binarna+di],dl
                dec     di
                cmp     ax,0
                jne     dziel2v2
                 
                cmp     di,0
                jl      koniec_intbin
                
           uzupelnij_zera2:
                mov     byte ptr [l_binarna+di],0
                dec     di
                cmp     di,0
                jge     uzupelnij_zera2     
                
           koniec_intbin:
                pop     si    
                pop     di     
                pop     dx
                pop     bx
                pop     ax     
                ret
;..............................

;in si ktora liczba
;[l_binarna]   
bin_na_ascii:                                ;zamienia l_binarna na kod ascii i zapisuje w tablica
                push    ax
                push    bx
                push    cx
                push    dx
                push    di
                push    si
                
                call    int16x16_to_bin
                
                mov     ax,si
                mov     bx,17
                mul     bx
                mov     si,ax
                push    si
                
                mov     cx,10                  
                
                xor     ax,ax                
                         
          oblicz_kolejne_cyfry1:
                mov     bx,offset potegi2
                mov     si,cx
                mov     di,31
          
          oblicz_kolejne_cyfry: 
                
                cmp     byte ptr [l_binarna+di],0
                je      pomin_bo_0                 
                                
                add     al,byte ptr [bx+si-1]
          pomin_bo_0:
                dec     di
                add     bx,10
                
                cmp     di,0
                jge     oblicz_kolejne_cyfry
                
               
                push    cx 
                
                mov     cx,10            
                xor     dx,dx                
                div     cx                     
                
                pop     cx
                mov     bx,cx
                                    
                add     dl,48
                
                pop     si                                    
                mov     byte ptr [tablica+si+bx+4],dl
                push    si 
                
                loop    oblicz_kolejne_cyfry1
                
                pop     si
                
               
                pop     si              
                pop     di     
                pop     dx
                pop     cx
                pop     bx
                pop     ax     
                ret

;..............................

zamien_bajty_na_ascii:
                push    cx
                push    dx
                push    si
                    
                mov     dx,seg bajty
                mov     es,dx
                
                mov     cx,256
                xor     si,si
                
        zamien_bajty:
                call    bin_na_ascii
                inc     si
                loop    zamien_bajty
                
                pop     si    
                pop     dx
                pop     cx
                ret
                               
;..............................

zapisz_do_pliku:
                push    ax
                push    bx
                push    cx
                push    dx
                
                mov     bx,uchwyt_zapis                  ;plik
                mov     cx,4420                          ;ile bajtow
                mov     dx,offset dane_plik              ;poczatek danych
                mov     ah,40h                           ;zapisz do pliku
                int     21h   
                
                pop     dx
                pop     cx
                pop     bx
                pop     ax
                ret

;..............................

;bx-uchwyt do pliku
zamknij_plik:
                push    ax
                mov     ah,03eh
                int     21h
                pop     ax
                ret 

;..............................   

code1 ends

stos1 segment stack

				dw		200 dup(?)							;rezerwacje 0-399 bitow
	w_stosu		dw 		?									;rezerwacja 400 bitu na wierzcholek

stos1 ends

end start