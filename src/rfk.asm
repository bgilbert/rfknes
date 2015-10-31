;
; rfk - robotfindskitten for NES
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

.include "nes.asm"
.include "string.asm"
.include "../nki/nki.asm"
.include "../chr/chr.asm"

.section zeropage
; Persistent state
nmi_done	.byte ?
.send

.section fixed
start	.proc
	.cp #$80, PPUCTRL ; configure PPU; enable NMI

	; set up arbitrary palette
	bit PPUSTATUS	; clear address latch
	.cp #$3f, PPUADDR
	.cp #$00, PPUADDR
	ldx #$10
-	stx PPUDATA
	inx
	cpx #$30
	bne -

	; print NKI
	.cp2 #690, tempA
	coord_addr = NAMETABLE_0 + 2 * 32 + 2
	.cp2 #coord_addr, tempB
	jsr print_nki

	; reset scroll after update
	bit PPUSTATUS	; clear address latch
	lda #0
	sta PPUSCROLL
	sta PPUSCROLL

main
-	lda nmi_done	; wait until after NMI
	beq -		; or loop
	.cp #0, nmi_done ; clear nmi_done
	jmp main	; NMI done; next iteration of main loop
	.pend

nmi	.proc
	pha		; Push A
	txa		; <- X
	pha		; Push X
	tya		; <- Y
	pha		; Push Y

	.cp #$1e, PPUMASK ; enable rendering
	.cp #1, nmi_done ; report NMI complete

	pla		; Pop Y
	tay		; -> Y
	pla		; Pop X
	tax		; -> X
	pla		; Pop A
	rti
	.pend

irq	.proc
	rti
	.pend

; Print an NKI
; tempA - NKI number
; tempB - nametable address
print_nki .proc
	asl tempA	; get table offset by doubling nki_num: low
	rol tempA + 1	; and high
	.cp #<nki_table, tempC ; copy low byte of table base to tempC.L
	lda #>nki_table	; load high byte
	clc		; clear carry
	adc tempA + 1	; add high byte of table offset
	sta tempC + 1	; and store it to tempC.H
	ldy tempA	; get low byte of offset for indirect indexed
	lda (tempC),y	; load low byte of string address
	sta tempA	; store to tempA.L
	iny		; increment offset for entry.H
	lda (tempC),y	; load entry.H
	tax		; copy to X
	lsr		; shift to recover high bits of string address
	lsr
	ora #$80	; fix the top two bits
	sta tempA + 1	; and store to tempA.H
	txa		; get entry.H back again
	and #$03	; and extract the bank
	jmp do_print	; tail call
	.pend
.send
