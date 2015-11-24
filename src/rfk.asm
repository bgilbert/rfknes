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
.include "board.asm"
.include "robot.asm"
.include "nki.asm"
.include "nmi.asm"
.include "../nki/nki.asm"
.include "../chr/chr.asm"

VERSION = "0.1"

.section zeropage
nmi_ready	.byte ?
cmd_off		.byte ?
rand_state	.word ?
prev_nki_y	.byte ?
prev_nki_lines	.byte ?
.send

.section bss
.align $100
cmd_buf		.fill $100
oam		.fill $100
.send

.strings instructions, 2, [format("  robotfindskitten v%s.%d", VERSION, nki_count), "     by Benjamin Gilbert", "       Original game by", "      Leonard Richardson", "   Released under the GPLv2", "", "", "In this game, you are robot.", "", "Your job is to find kitten.", "", "This task is complicated by", "the existence of various", "things which are not kitten.", "", "Robot must touch items to", "determine if they are kitten", "or not.", "", "The game ends when", "robotfindskitten.", "", "", "         PRESS START"]

.section fixed
palette
	.byte $0f, $10, $10, $10
	.byte $0f, $1a, $21, $24
	.byte $0f, $16, $1c, $2a
	.byte $0f, $13, $18, $26

start	.proc
	.cp2 #$c292, rand_state ; initialize random state
	.cp #>NAMETABLE_0, nametable ; initialize nametable
	.cp #$80, PPUCTRL ; configure PPU; enable NMI

	; initialize OAM buffer and OAM
	ldx #0		; counter
	clc		; clear carry
-	lda #$ff	; Y coordinate (off-screen)
	sta oam,x	; store
	lda #0
	sta oam + 1,x	; store glyph 0
	sta oam + 2,x	; store no attributes
	sta oam + 3,x	; store X coordinate
	txa		; get counter
	adc #4		; increment for next sprite
	tax		; put back
	bne -		; continue until done
	lda #ROBOT	; robot glyph
	sta oam + 1	; store in sprite 0
	ldy #0		; init cmd_buf offset
	.ccmd #CMD_OAM	; copy OAM buf to OAM
	sty cmd_off	; update offset

	; copy fixed palette to background and sprite palettes
	bit PPUSTATUS	; clear address latch
	.cp #>PALETTE_BG, PPUADDR ; address high
	.cp #<PALETTE_BG, PPUADDR ; address low
	ldy #2		; outer loop counter
-	ldx #0		; inner loop counter
-	lda palette,x	; get palette value
	sta PPUDATA	; write it
	inx		; increment inner counter
	cpx #$10	; are we done?
	bne -		; no; continue
	dey		; decrement outer counter
	bne --		; continue until done

	; render instructions
	.print NAMETABLE_0, 2, 3, instructions

	; enable render
	ldy cmd_off	; get offset
	.ccmd #CMD_POKE	; command
	.ccmd #<PPUMASK	; addr low byte
	.ccmd #>PPUMASK	; addr high byte
	.ccmd #PPUMASK_NORMAL ; enable rendering
	sty cmd_off	; update offset
	jsr run_nmi	; draw

	; wait for Start button; generate board
	jsr wait_for_start
	jsr clear_nametable
	jsr make_board
	jsr place_robot

main	jsr do_input	; handle player actions
	jsr run_nmi	; wait for NMI
	jmp main	; continue main loop
	.pend

direction_mask .byte ((1 << BTN_UP) | (1 << BTN_DOWN) | (1 << BTN_LEFT) | (1 << BTN_RIGHT))

; Repeatedly increment PRNG state until Start button is pressed
wait_for_start .proc
-	inc rand_state	; increment rand_state.L
	bne +		; need to carry?
	inc rand_state + 1 ; yes; increment rand_state.H
	bne +		; rand_state == 0?
	inc rand_state	; yes; increment again to avoid stuck PRNG
+	jsr input	; query buttons
	lda new_buttons	; get result
	.cbit BTN_START	; check for start button
	beq -		; continue if not pressed
	rts
	.pend

; Respond to user actions
do_input .proc
	; query buttons
	jsr input	; read input
	lda new_buttons	; get result

	; Select generates new board
	.cbit BTN_SELECT ; check Select button
	beq +		; continue unless pressed
	jmp next_board	; create new board

	; Start pauses
+	.cbit BTN_START	; check Start button
	beq +		; continue unless pressed
	jmp pause	; pause

	; return if directional button not pressed
+	bit direction_mask ; check buttons
	bne +		; OK if at least one pressed
	rts		; else bail

	; load coords
+	ldx robot_x	; X coord
	ldy robot_y	; Y coord

	; check left
	.cbit BTN_LEFT	; check bit
	beq +		; or continue
	dex		; decrement

	; check right
+	.cbit BTN_RIGHT	; check bit
	beq +		; or continue
	inx		; increment

	; check up
+	.cbit BTN_UP	; check bit
	beq +		; or continue
	dey		; decrement

	; check down
+	.cbit BTN_DOWN	; check bit
	beq +		; or continue
	iny		; increment

	; apply bounds
+	cpx #BOARD_X_OFFSET ; X coord < min?
	bpl +		; no
	ldx robot_x	; or reset
+	cpx #(BOARD_X_THRESHOLD + BOARD_X_OFFSET) ; X coord > max?
	bmi +		; no
	ldx robot_x	; or reset
+	cpy #BOARD_Y_OFFSET ; Y coord < min?
	bpl +		; no
	ldy robot_y	; or reset
+	cpy #(BOARD_Y_THRESHOLD + BOARD_Y_OFFSET) ; Y coord > max?
	bmi +		; no
	ldy robot_y	; or reset

	; check for new item
+	stx cur_x	; X coord argument
	sty cur_y	; Y coord argument
	jsr get_bit_position ; get bitmap index
	lda item_bitmap,x ; load bitmap byte
	bit bit_mask	; test bit
	beq clear	; jump if clear

	; check whether new item is kitten
	jsr test_kitten	; is item kitten?
	bne +		; jump if not kitten
	jmp next_board	; kitten; replace board and return

	; if new item is NKI with no current NKI showing, handle and return
+	lda nki_lines	; see if we're currently showing an NKI
	bne +		; yes; continue
	jmp show_nki	; else show NKI and return

	; need to replace current NKI
+	sta prev_nki_lines ; save current NKI line count
	lda nki_y	; get current NKI Y coord
	sta prev_nki_y	; save it
	jsr show_nki	; draw new NKI
	lda #16		; mid-screen threshold
	cmp prev_nki_y	; compare to previous Y
	bpl top		; previous NKI at top of screen
	cmp nki_y	; compare to current Y
	bpl mixed	; mixed top and bottom
	; both at bottom; if new nki_y > old, fix difference
	lda prev_nki_y	; get old top
	cmp nki_y	; compare to new top
	bpl return	; new <= old; done
	sta start_y	; start Y coord for clearing
	.cp nki_y, end_y ; end Y coord for clearing
	bne redraw	; redraw
top	cmp nki_y	; compare to current NKI
	bmi mixed	; mixed top and bottom
	; both at top; if new nki_lines < old, fix difference
	lda nki_lines	; get new line count
	cmp prev_nki_lines ; compare to old line count
	bpl return	; new >= old; done
	clc		; clear carry
	adc nki_y	; add new Y coord
	sta start_y	; start Y coord for clearing
	lda prev_nki_lines ; get old line count
	adc prev_nki_y	; add old Y coord
	sta end_y	; end Y coord for clearing
	bne redraw	; redraw items
	; mixed; completely clean old NKI
mixed	lda prev_nki_y	; start Y coord for redraw
	sta start_y	; store it
	clc		; clear carry
	adc prev_nki_lines ; add length
	sta end_y	; store end line
	jsr run_nmi	; wait for frame, since draw + clear is too much
			; work for one frame
redraw	jsr clear_lines	; clear
	jsr draw_board	; redraw items
return	rts

	; no new NKI; clear old NKI if showing
clear	ldx cur_x	; restore X coord
	ldy cur_y	; restore Y coord
	lda nki_lines	; see if an NKI is showing
	beq +		; no; continue
	stx temp1	; save X coord
	sty temp2	; save Y coord
	clc		; clear carry
	adc nki_y	; compute ending Y coord of current NKI
	sta end_y	; store for board redraw
	lda nki_y	; get starting Y coord
	sta start_y	; and store it
	jsr clear_lines	; clear NKI
	.cp #0, nki_lines ; clear NKI indication
	jsr draw_board	; redraw board
	jsr run_nmi	; draw frame
	ldx temp1	; restore X coord
	ldy temp2	; restore Y coord

	; update state
+	lda robot_x	; get old X coord
	sta cur_x	; store argument
	lda robot_y	; get new Y coord
	sta cur_y	; store argument
	stx robot_x	; write new X coord
	sty robot_y	; write new Y coord

	; update robot
	lda robot_x	; get X coord back
	sta cur_x	; save argument
	lda robot_y	; get Y coord back
	sta cur_y	; save argument
	jmp draw_robot	; draw the robot
	.pend

pause	.proc
	; gray out screen
	ldy cmd_off	; buffer offset
	.ccmd #CMD_POKE	; command
	.ccmd #<PPUMASK	; addr low byte
	.ccmd #>PPUMASK	; addr high byte
	.ccmd #(PPUMASK_NORMAL | $1) ; rendering enabled; grayscale
	sty cmd_off	; update offset

	; wait for Start
-	jsr run_nmi	; draw
	jsr input	; read input
	lda new_buttons	; get result
	.cbit BTN_START	; check Start button
	beq -		; continue unless pressed

	; ungray screen
	ldy cmd_off	; buffer offset
	.ccmd #CMD_POKE	; command
	.ccmd #<PPUMASK	; addr low byte
	.ccmd #>PPUMASK	; addr high byte
	.ccmd #PPUMASK_NORMAL ; rendering enabled
	sty cmd_off	; update offset
	rts
	.pend

; Show next board
next_board .proc
	lda nki_lines	; see if an NKI is showing
	beq +		; no; continue
	clc		; clear carry
	adc nki_y	; compute end Y coord of NKI
	sta end_y	; and store it
	lda nki_y	; get start Y coord
	sta start_y	; and store it
	jsr clear_lines	; clear NKI
	.cp #0, nki_lines ; clear NKI indication
	jsr run_nmi	; wait for frame
+	jsr end_board	; clear board
	lda robot_x	; load X coord
	sta cur_x	; store argument
	lda robot_y	; load Y coord
	sta cur_y	; store argument
	jsr make_board	; make a new board
	jsr place_robot	; place the robot
	.pend

; Tell NMI handler we're ready, then wait for it to complete
; Clobbers: A, Y
run_nmi .proc
	; terminate buffer
	ldy cmd_off	; buffer index
	.ccmd #CMD_EOF	; write command

	; enable NMI and wait for it
	.cp #1, nmi_ready ; tell NMI handler to proceed
-	lda nmi_ready	; wait until after NMI
	bne -		; or loop
	.cp #0, cmd_off	; reset cmd_buf offset
	rts
	.pend

irq	.proc
	jmp reset	; can't happen, so give up
	.pend

; Generate "random" number
; 16-bit Galois LFSR; polynomial fc00
; Return: number in A
; Clobbers: A, X
rand	.proc
	lda rand_state + 1 ; load high byte of state
	ldx #8		; initialize round counter

-	lsr		; shift high byte
	ror rand_state	; shift low byte
	bcc +		; see if output bit is 1
	eor #$fc	; yes, so xor poly
+	dex		; decrement round counter
	bne -		; continue if not done

	sta rand_state + 1 ; update high byte
	lda rand_state	; return low byte
	rts
	.pend

; Store A to command buffer indexed by Y; increment Y
cmd	.macro
	sta cmd_buf,y
	iny
	.endm

; Copy value to command buffer indexed by Y; increment Y
; args: value
ccmd	.macro
	lda \1
	.cmd
	.endm
.send
