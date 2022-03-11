.386
.model flat, stdcall

includelib msvcrt.lib
extern exit: proc
extern printf: proc
extern scanf :proc
extern gets: proc
extern puts: proc
extern sscanf: proc
extern isdigit: proc

public start

.data
stiva_termeni dq 35 dup(0);  stiva de termeni  --1
stiva_operatii db 35 dup(0); stiva de operatii --2


format0 db 10,13,"Introduceti o expresie:",0
format1 db "%lf",0
format2 db 10,13,"Expresie invalida.",0
string db 200 dup (0);


top_termeni dd 0; pozitia varfului stivei de termeni
top_operatii dd 0; pozitia varfului stivei de operatii
rez dq 0
.code

apel_f3 macro functie,x,y,z
	push z
	push y
	push x
	call functie
endm

push_stiva PROC; se pune pe pozitia stiva[top], iar top se incrementeaza corespunzator
	push EBP
	mov EBP, ESP
	push EAX;salvam valorile registrilor utilizati
	push EBX
	push ESI
	mov EBX, [EBP+8]
	mov EAX, [EBP+12]; valoarea va fi salvata in EBX:EAX
	mov ESI, [EBP+16]; 1 sau 2
	
	cmp ESI,1; vedem pe care stiva trebuie sa punem
	je stiva_1
	
	stiva_2: 
	mov ESI,top_operatii
	mov stiva_operatii[ESI],BL
	add ESI, type stiva_operatii
	mov top_operatii,ESI
	jmp done
	
	stiva_1:
	mov ESI,top_termeni
    mov dword ptr stiva_termeni[ESI],EBX
	mov dword ptr stiva_termeni[ESI+4],EAX
	add ESI, type stiva_termeni
	mov top_termeni,ESI
	
	done:
	pop ESI; returnam valorile precedente ale registrilor
	pop EBX
	pop EAX
	mov ESP, EBP
	pop EBP
	ret 12
push_stiva ENDP

pop_stiva PROC; se extrage elementul de la pozitia stiva[top-1] sau stiva[top-8]; se decrementeaza apoi top
	push EBP
	mov EBP, ESP
	push ESI; salvam ESI
	mov EAX, [EBP+8]
	
	cmp EAX,1; vedem de pe care stiva trebuie sa scoatem
	je stiva_1
	
	stiva_2: 
	mov ESI,top_operatii
	sub ESI, type stiva_operatii
	mov top_operatii,ESI ;decrementam indexul varfului
	mov AL,stiva_operatii[ESI]; in AL returneaza operatia
	jmp done
	
	stiva_1:
	mov ESI,top_termeni
	sub ESI, type stiva_termeni
	mov top_termeni,ESI ;decrementam indexul varfului
	mov EAX,dword ptr stiva_termeni[ESI+4]
	mov EBX, dword ptr stiva_termeni[ESI]; in EBX:EAX returneaza termenul
	
	done:
	pop ESI
	mov ESP, EBP
	pop EBP
	ret 4
pop_stiva ENDP

load_from_stack macro ; muta din stiva de termeni in stiva coprocesorului
	LOCAL over, fin
	push EAX
	push EBX; salvam registii folositi
	push 1
	call pop_stiva
	cmp EAX,"pppp"
	jne over
	cmp EBX,"pppp"; verific daca numarul este "pppppppp", adica po
	jne over
	fldpi
	jmp fin
	over: 
	mov dword ptr [rez+4],EAX
	mov dword ptr [rez],EBX
	fld rez
	fin:
	pop EBX
	pop EAX
endm

adun PROC
	push EBP
	mov EBP, ESP
	load_from_stack; fac pop de 2 ori din stiva de termeni, incarcand in coprocesor
	load_from_stack
	fadd
	fstp rez 
	apel_f3 push_stiva,dword ptr [rez],dword ptr[rez+4],1; pun pe stiva de termeni rezultatul extras din stiva coprocesorului
	mov ESP, EBP
	pop EBP
	ret 
adun ENDP

scad PROC
	push EBP
	mov EBP, ESP
	load_from_stack
	load_from_stack
	fsubr
	fstp rez
	apel_f3 push_stiva,dword ptr [rez],dword ptr[rez+4],1
	mov ESP, EBP
	pop EBP
	ret 
scad ENDP

inm PROC
	push EBP
	mov EBP, ESP
	load_from_stack
	load_from_stack
	fmul
	fstp rez
	apel_f3 push_stiva,dword ptr [rez],dword ptr[rez+4],1
	mov ESP, EBP
	pop EBP
	ret 
inm ENDP

imp PROC
	push EBP
	mov EBP, ESP
	load_from_stack
	load_from_stack
	fdivr
	fstp rez
	apel_f3 push_stiva,dword ptr [rez],dword ptr[rez+4],1
	mov ESP, EBP
	pop EBP
	ret 
imp ENDP

sin PROC
	push EBP
	mov EBP, ESP
	load_from_stack; functiile de rank 3 au nevoie doar de un termen
	fsin
	fstp rez
	apel_f3 push_stiva,dword ptr [rez],dword ptr[rez+4],1
	mov ESP, EBP
	pop EBP
	ret 
sin ENDP

log PROC
	push EBP
	mov EBP, ESP
	fld1
	load_from_stack
	fyl2x  ; avem in varful stivei log2(nr)
	fldl2t ; punem in varful stivei log2(10)
	fdiv   ; ST(0) va avea valoarea: log2(nr)/log2(10) = log10(nr)
	fstp rez
	apel_f3 push_stiva,dword ptr [rez],dword ptr[rez+4],1
	mov ESP, EBP
	pop EBP
	ret 
log ENDP

doOperation PROC
	push EBP
	mov EBP, ESP
	push EAX; salvam valoarea lui EAX
	mov EAX, [EBP+8]
	
	cmp EAX,'+'
	je op_add
	cmp EAX,'-'
	je op_sub
	cmp EAX,'*'
	je op_mul	
	cmp EAX, '/'
	je op_div	
	cmp EAX,'s'
	je op_sin	
	cmp EAX,'l'
	je op_log	
	jmp done
	
	op_add: 
	call adun
	jmp done
	op_sub: 
	call scad
	jmp done
	op_mul:
	call inm
	jmp done
	op_div:
	call imp
	jmp done
	op_sin:
	call sin
	jmp done
	op_log:
	call log
	
	done:
	pop EAX; punem valoarea precedenta a lui EAX
	mov ESP, EBP
	pop EBP
	ret 4
doOperation ENDP

getRank PROC; in eax returneaza rank-ul
	push EBP
	mov EBP, ESP
	mov EAX, [EBP+8]
	cmp EAX,'+'
	je rank1
	cmp EAX,'-'
	je rank1
	cmp EAX,'*'
	je rank2
	cmp EAX, '/'
	je rank2
	cmp EAX,'s'; sin
	je rank3
	cmp EAX,'l'; log
	je rank3
	xor eax,eax;daca nu e operatie atunci nu are rank si semnalam punand 0 in eax
	jmp done
	rank1: 
	mov EAX,1
	jmp done
	rank2: 
	mov EAX,2
	jmp done
	rank3:
	mov EAX,3
	jmp done
	done: 
	mov ESP, EBP
	pop EBP
	ret 4
getRank ENDP

getNumber PROC;returneaza numarul in EAX
	push EBP
	mov EBP, ESP
	sub ESP,8
	push EDX;salvam val lui edx(care se modifica la apelul functiei sscanf)
	lea EAX,[EBP-8];tritem adresa efectiva ca si parametru pt sscanf (aici vom stoca numarul)[var. locala pt functie]
	push EAX
	push offset format1
	push [EBP+8]
	call sscanf
	add ESP,12
	mov EAX, [EBP-4]
	mov EBX, [EBP-8]
	pop EDX
	mov ESP, EBP
	pop EBP
	ret 4
getNumber ENDP

parcurg PROC
	push EBP
	mov EBP, ESP
	new_string:
	push offset format0
	call puts
	add ESP,4
	push offset string
	call gets
	add ESP,4
	
	;verificam daca este exit
	xor ESI,ESI
	xor EAX,EAX
	mov AL,string[ESI]
	cmp AL,'e'
	jne evaluate_first
	mov AL,string[ESI+1]
	cmp AL,'x'
	jne invalid
	mov AL,string[ESI+2]
	cmp AL,'i'
	jne invalid
	mov AL,string[ESI+3]
	cmp AL,'t'
	je fin
	jmp invalid; daca nu sare la final, atunci string-ul este invalid

	evaluate_first: ;verificam daca string-ul incepe cu operatie de rank 1, 2 sau '('
	xor EAX,EAX
	mov AL,string[ESI]
	cmp AL,'('
	je delete_prev_rez
	push EAX
	call getRank
	cmp EAX,0
	je delete_prev_rez
	cmp EAX,3
	je delete_prev_rez
	jmp begin
	
	delete_prev_rez: ;daca noul string incepe cu operand sau paranteza, sau sin sau log, rezultatul anterior nu ne mai intereseaza
	push 1
	call pop_stiva
	
	begin:
	xor EDX,EDX
	xor ESI,ESI
	
	get_next:
	
	;verificam daca am ajuns la finalul expresiei
	mov DL,string[ESI]
	cmp DL,'='
	je final_op
	
	;verificam daca este spatiu
	cmp DL,' '
	jne check_for_pi
	inc ESI
	jmp get_next;daca a fost spatiu, continuam parcurgerea
	
	;verificam daca este pi
	check_for_pi:
	cmp DL,'p'
	jne continue1
	inc ESI
	mov DL,string[ESI]
	cmp DL,'i'
	jne invalid; daca primul caracter a fost p si al doilea nu este i, string-ul este invalid
	inc ESI
	mov EAX,"pppp"
	apel_f3 push_stiva,EAX,EAX,1; semnalam pi ca fiind un dq de forma "pppppppp"
	jmp get_next
	
	;verificam daca este numar
	continue1:
	push EDX
	call isdigit
	add ESP,4
	cmp EAX,0; isdigit returneaza 0 in EAX daca nu este cifra
	je continue2
	mov EAX, offset string
	add EAX,ESI
	
	push EAX
	call getNumber; in EBX:EAX se va returna numarul
	apel_f3 push_stiva,EBX,EAX,1; punem numarul pe stiva termenilor 
	;cat timp este cifra sau '.' continuam incrementarea ESI, pentru parcurgerea stringului
	pozitii_ocupate:
		inc ESI
		mov DL,string[ESI]
		cmp DL,'.'
		je pozitii_ocupate
		push EDX
		call isdigit
		add ESP,4
		cmp EAX,0
		je get_next
	jmp pozitii_ocupate
	
	continue2:
	;verificam daca este operatie
	push EDX
	call getRank
	
	cmp EAX,0; daca functia returneaza zero in EAX inseamna ca nu e operatie
	je continue3
	mov EBX,EAX; salvam in EBX rank-ul operatiei curente din string
	cmp EBX,3
	jne loop1
	;deoarece operatia are rang 3,aceasta se scrie pe 3 caractere, si verific daca a fost introdusa corect
	mov ECX,EDX; salvam operatia de pus pe stiva 
	
	;verific daca este sin
	check_sin:
	cmp DL,'s'
	jne check_log
	inc ESI
	mov DL,string[ESI]
	cmp DL,'i'
	jne invalid
	inc ESI
	mov DL,string[ESI]
	cmp DL,'n'
	jne invalid
	mov EDX,ECX;returnam in edx valoarea sa precedenta care se modifica in cadrul verificarii operatiilor
	jmp loop1;
	
	;verific daca este log
	check_log: 
	cmp DL,'l'
	jne invalid
	inc ESI
	mov DL,string[ESI]
	cmp DL,'o'
	jne invalid
	inc ESI
	mov DL,string[ESI]
	cmp DL,'g'
	jne invalid
	
	mov EDX,ECX; returnam in edx valoarea sa precedenta
	
	loop1:
		mov ECX,top_operatii
		cmp ECX,0; cat timp stiva de operatii nu este goala
		je done_applying
		mov edi,top_operatii
		sub edi,1
		xor eax,eax
		mov AL,stiva_operatii[EDI]; "peek" la varful stivei, fara a face pop
		push EAX
		call getRank ;in eax se va afla rankul operatiei din varful stivei
		cmp EAX,EBX; cat timp operatia curenta nu are rank prioritar fata de cea din varful stivei
		jl done_applying
		push 2
		call pop_stiva
		;in eax se va afla operatia din varful stivei
		push EAX
		call doOperation; efectuam operatia
	jmp loop1
	
	done_applying: ;dupa iesirea din bucla
	apel_f3 push_stiva, EDX, 0, 2 ; punem pe stiva de operatii operatia gasita 
	;al 2-lea argument este irelevant pentu atunci cand punem in operatii, de aceea am pus 0
	inc ESI; parcurgem sirul de la pozitia urmatoare
	jmp get_next
	
	continue3:
	;verificam daca este paranteza deschisa
	cmp DL,'('
	jne continue4
	apel_f3 push_stiva,EDX,0,2; punem pe stiva de operatii paranteza '('
	
	mov DL,string[ESI+1]; in caz ca paranteza incepe cu operatie ( de forma (+1) sau (-1)), pun 0 pe stiva de termeni
	cmp DL,'+'
	je aide
	cmp DL,'-'
	je aide
	jmp not_needed
	
	aide: 
	apel_f3 push_stiva,0,0,1
	
	not_needed:
	inc ESI
	jmp get_next
	
	continue4:
	;verificam daca este paranteza inchisa
	cmp DL,')'
	jne invalid; daca nu este nici operatie,operand,paranteza,egal, atunci string-ul introdus contine un caracter invalid
	
	loop2: 
		mov EDI,top_operatii
		xor ECX,ECX
		mov CL,stiva_operatii[EDI-1]; salvam in CL elementul din varful stivei de operatii
		cmp CL,'('; cat timp in varful stivei de operatii nu este '('
		je done_loop2
		push 2
		call pop_stiva
		;in eax se va afla operatia din varful stivei
		push EAX
		call doOperation
	jmp loop2
	
	done_loop2: ;dupa ce am iesit din bucla, eliminam paranteza '(' ramasa pe stiva de operatii
	push 2
	call pop_stiva
	inc ESI; continuam parcurgerea string-ului
	jmp get_next
	
	;dupa ce am ajuns la '=', cat timp stiva de operatii nu e goala, facem pop si efectuam operatiile
	final_op:
		mov EDI, top_operatii
		cmp EDI,0
		je final_result; daca stiva de operatii este goala, au fost efectuate toate operatiile, afisam rezultatul
		push 2
		call pop_stiva
		;in eax se va afla operatia din varful stivei
		push EAX
		call doOperation
	jmp final_op
	
	;in stiva de termeni va ramane un singur termen, adica rezultatul, pe care il afisam
	final_result:
	xor ESI,ESI
	push dword ptr stiva_termeni[ESI+4]
	push dword ptr stiva_termeni[ESI]
	push offset format1
	call printf
	jmp new_string
	
	invalid:
	push offset format2
	call puts
	add ESP,4
	mov eax,0
	mov top_operatii,eax; "curatam" stiva de operatii
	mov top_termeni,eax; "curatam" stiva de termeni
	apel_f3 push_stiva,0,0,1; punem in varful stivei de termeni zero
	jmp new_string
	
	fin:
	mov ESP, EBP
	pop EBP
	ret
parcurg ENDP

start:
	finit
	apel_f3 push_stiva,0,0,1; punem 0 ca prim element in caz ca string-ul incepe cu operatie de rank 1 sau 2: +,-,*,/ la prima introducere
	call parcurg
    push 0
	call exit
end start
