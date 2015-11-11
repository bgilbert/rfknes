;
; nes - Generic NES support
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

.cpu "6502i"

prg_banks = (prg_end - prg_start) / 16384
chr_banks = (chr_end - chr_start) / 8192

; iNES header
.byte	($4e, $45, $53, $1a)	; Magic
.byte	prg_banks		; PRG ROM size, 16 KB units
.byte	chr_banks		; CHR ROM size, 8 KB units
.byte	2 << 4			; Mapper 2
.fill	9

prg_start =	*

; PRG ROM variable banks
.logical $8000
.dsection bank0
.fill 1
.align	$4000, 0
.cerror	* != $c000, "Incorrect bank 0 size"
.here

.logical $8000
.dsection bank1
.fill 1
.align	$4000, 0
.cerror	* != $c000, "Incorrect bank 1 size"
.here

.logical $8000
.dsection bank2
.fill 1
.align	$4000, 0
.cerror	* != $c000, "Incorrect bank 2 size"
.here

; PRG ROM fixed bank
.logical $c000
.dsection fixed
banknums .byte range(prg_banks)	; for avoiding bus conflicts in bank swaps
.warn	"Code size: ", (* - $c000), " bytes"
* =	$fffa
.word	(nmi, reset, irq)
.cerror	* != 0, "Incorrect fixed bank size"
.here

prg_end =	*

; BSS
.logical 0
temp1	.byte ?
temp2	.byte ?
temp3	.byte ?
temp4	.byte ?
temp5	.byte ?
temp6	.byte ?
temp7	.byte ?
temp8	.byte ?
tempA	.word ?
tempB	.word ?
tempC	.word ?
tempD	.word ?
.dsection zeropage
.cerror	* > $100
.here
.logical $200
.dsection bss
.cerror * > $7ff
.here

* =		prg_end		; don't include BSS in output
chr_start =	*

; CHR ROM
.logical 0
.dsection chr
.align	$2000, 0
.cerror	* != $2000, "Incorrect CHR ROM size"
.here

chr_end =	*

; Constants
PPUCTRL =	$2000
PPUMASK =	$2001
PPUSTATUS =	$2002
OAMADDR =	$2003
OAMDATA =	$2004
PPUSCROLL =	$2005
PPUADDR =	$2006
PPUDATA =	$2007
OAMDMA =	$4014

NAMETABLE_0 =	$2000
NAMETABLE_1 =	$2400
NAMETABLE_2 =	$2800
NAMETABLE_3 =	$2c00

; Init
.section fixed
reset	.proc
	; Force interrupts off (i.e., for soft reset)
	sei		; Disable CPU interrupts if enabled
	lda #0
	sta PPUCTRL	; Disable PPU NMI (if PPU already warm)
	sta $4015	; Disable APU channels
	sta $4010	; Disable APU DMC interrupt
	.cp #$40, $4017 ; Disable APU frame interrupt
	cli		; Re-enable interrupts

	; Set up CPU
	cld		; disable BCD (inert but recommended)
	ldx #$ff
	txs		; set stack pointer

	; Wait for first half of PPU warmup
	bit PPUSTATUS	; clear PPUSTATUS VBL
-	bit PPUSTATUS	; check PPUSTATUS VBL
	bpl -		; loop until set

	; Now that we're in vblank, disable rendering if PPU is already warm
	lda #0
	sta PPUMASK	; disable rendering

	; Clear RAM
	sta tempA	; low byte of base address
	sta tempA + 1	; high byte of base address
	ldx #0		; high byte of base address (reg)
	ldy #2		; index
-	sta (tempA),y	; write byte
	iny		; increment index
	bne -		; loop until Y overflows
	inx		; increment base address (high byte)
	stx tempA + 1	; and store it
	cpx #8		; we're done at $0800
	bne -		; loop if not done

	; Wait for rest of PPU warmup
-	bit PPUSTATUS	; check PPUSTATUS VBL
	bpl -		; loop until set

	jmp start
	.pend

; Copy value to location, clobbering A
; args: value, location
cp	.macro
	lda \1
	sta \2
	.endm

; Copy 16-bit value to location, clobbering A
; args: value, location
cp2	.macro
	value = \1
	lda <value
	sta \2
	lda >value
	sta \2 + 1
	.endm

.send
