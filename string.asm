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

; Define new string array (double-null-terminated)
; args: string_name, bank, [string]
strings	.segment
\1_bank = \2
	.section bank\2
\1_addr	.null \3[0]
	.for _cur = 1, _cur < len(\3), _cur = _cur + 1
	.null \3[_cur]
	.next
	.byte 0
	.send
	.endm

; Define new string (double-null-terminated)
; args: string_name, bank, string
string	.segment
	.strings \1, \2, [\3]
	.endm

; Print a string array
; args: nametable_base, x, y, bank, addr
print	.macro
	.cp #<\5, tempA
	.cp #>\5, tempA + 1
	coord_addr = \1 + \3 * 32 + \2
	.cp #<coord_addr, tempB
	.cp #>coord_addr, tempB + 1
	lda #\4
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

	; print line
next	bit PPUSTATUS	; clear latch
	lda nt_addr + 1	; load high byte
	sta PPUADDR	; write it
	lda nt_addr	; load low byte
	sta PPUADDR	; write it
	ldy #0		; initialize index
	jmp +		; start loop
-	sta PPUDATA	; store character
	iny		; increment counter
+	lda (addr),y	; load character
	bne -		; continue until NUL

	;  return if done
	cpy #0		; did the string have 0 characters?
	bne +
	rts

	; update string pointer
+	tya		; move count to accumulator
	sec		; set carry to account for null byte
	adc addr	; add to string address
	sta addr	; write it back
	bcc +		; need to update high byte?
	ldx addr + 1	; yes; load,
	inx		; increment,
	stx addr + 1	; and write back

	; update nametable pointer
+	lda #32		; one full row == 32 bytes
	clc		; clear carry
	adc nt_addr	; add to nametable address
	sta nt_addr	; write it back
	bcc +		; need to update high byte?
	ldx nt_addr + 1	; yes; load,
	inx		; increment,
	stx nt_addr + 1	; and write back

+	jmp next	; loop
	.pend
.send
