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
.dsection bank3		; for strings
.dsection fixed
banknums .byte range(prg_banks)	; for avoiding bus conflicts in bank swaps
* =	$fffa
.word	(nmi, reset, irq)
.cerror	* != 0, "Incorrect fixed bank size"
.here

prg_end =	*

; BSS
.logical 0
temp1	.byte ?
temp2	.byte ?
tempA	.word ?
tempB	.word ?
buttons		.byte ?
new_buttons	.byte ?
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
;.align	$2000, 0
;.cerror * != $2000, "Incorrect CHR ROM size"
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
PALETTE_BG =	$3f00
PALETTE_SPRITE = $3f10

PPUMASK_NORMAL = $1e

; bit numbers
BTN_A =		7
BTN_B =		6
BTN_SELECT =	5
BTN_START =	4
BTN_UP =	3
BTN_DOWN =	2
BTN_LEFT =	1
BTN_RIGHT =	0

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

	; Clear CPU RAM
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

	; Clear PPU RAM
	lda #0		; get zero
	sta PPUADDR	; store high byte
	sta PPUADDR	; store low byte
	ldx #0		; counter high byte
	ldy #0		; counter low byte
-	sta PPUDATA	; write byte
	iny		; increment counter.L
	bne -		; continue until low byte overflow
	inx		; increment counter.H
	cpx #$30	; stop at $3000
	bne -		; continue until done
	bit PPUSTATUS	; clear address latch
	.cp #$3f, PPUADDR ; palette RAM address high byte
	.cp #$0, PPUADDR ; palette RAM address low byte
	ldx #$20	; size of palette RAM
	lda #0		; data value
-	sta PPUDATA	; write byte
	dex		; decrement counter
	bne -		; continue until done

	jmp start
	.pend

; Read controller 1
; Return:
; buttons - buttons currently down
; new_buttons - newly-pressed buttons
input	.proc
	.cp #1, $4016	; strobe controller
	.cp #0, $4016	; stop strobing
	ldx #8		; initialize bit counter
-	lda $4016	; load bit from controller
	ror		; move into carry
	rol new_buttons	; and rotate into new_buttons
	dex		; decrement bit counter
	bne -		; loop until done

	ldx new_buttons	; get current buttons
	lda buttons	; and previous ones
	eor #$ff	; complement previous buttons
	and new_buttons	; and compute which ones have changed
	sta new_buttons	; record them
	stx buttons	; store current buttons
	rts
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

; Use bit instruction to check specified bit against A
; args: bit number
cbit	.macro
	bit left_shifts + \1
	.endm

left_shifts
	.for index = 0, index < 8, index = index + 1
	.byte (1 << index)
	.next

.send
