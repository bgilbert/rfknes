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

.strings hello, 2, ["Hello,", "NES"]
.string world, 2, "world."

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

	; hello world
	.print NAMETABLE_0, 11, 13, hello_bank, hello_addr
	.print NAMETABLE_0, 15, 14, world_bank, world_addr

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
.send
