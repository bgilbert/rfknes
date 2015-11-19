;
; string - String handling
;
; Copyright (C) 2015 Benjamin Gilbert
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License along
; with this program; if not, write to the Free Software Foundation, Inc.,
; 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
;

; Define new string array (number of strings, followed by null-terminated
; strings)
; args: string_name, bank, [string]
strings	.segment
\1_bank = \2
	.section bank\2
\1_addr	.byte len(\3)
	.for _cur = 0, _cur < len(\3), _cur = _cur + 1
	.null \3[_cur]
	.next
	.send
	.endm

; Define new string (double-null-terminated)
; args: string_name, bank, string
string	.segment
	.strings \1, \2, [\3]
	.endm

; Record string in string table
; args: string_name
stringentry .macro
	.byte <\1_addr
	.byte ((\1_addr >> 6) & $fc) | \1_bank
	.endm

; Print a string array
; args: nametable_base, x, y, symbol
print	.macro
	.cp2 #\4_addr, tempA
	coord_addr = \1 + \3 * 32 + \2
	.cp2 #coord_addr, tempB
	lda #\4_bank
	jsr do_print
	.endm

.section fixed
; Print a string array
; A - bank
; tempA - string address
; tempB - nametable address
do_print .proc
	addr = tempA
	nt_addr = tempB

	; switch banks
	tax		; copy bank to X
	sta banknums,x	; switch bank, avoiding bus conflicts

	; get number of lines
	ldy #0		; initialize index
	lda (addr),y	; load number of lines
	tax		; put in X
	iny		; increment index

	; print line
next	bit PPUSTATUS	; clear latch
	lda nt_addr + 1	; load high byte
	sta PPUADDR	; write it
	lda nt_addr	; load low byte
	sta PPUADDR	; write it
	jmp +		; start loop
-	sta PPUDATA	; store character
	iny		; increment index
+	lda (addr),y	; load character
	bne -		; continue until NUL
	dex		; decrement lines remaining

	; break if done
	cpx #0		; are there any lines remaining?
	beq done

	; update string pointer
+	tya		; move count to accumulator
	adc addr	; add to string address.  carry must be set to
			; account for null byte; already set by cpx #0 above
	sta addr	; write it back
	bcc +		; need to update high byte?
	inc addr + 1	; yes

	; update nametable pointer
+	lda #32		; one full row == 32 bytes
	clc		; clear carry
	adc nt_addr	; add to nametable address
	sta nt_addr	; write it back
	bcc +		; need to update high byte?
	inc nt_addr + 1	; yes

+	ldy #0		; reset index
	jmp next	; loop

done	rts
	.pend
.send
