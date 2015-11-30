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

; Debug options
INDICATE_KITTEN = 0	; use smiley-face glyph for kitten
SHOW_BOUNDARY = 0	; show boundary between screen top and bottom

.section zeropage
nmi_ready	.byte ?
cmd_off		.byte ?
rand_state	.word ?
prev_nki_y	.byte ?
prev_nki_lines	.byte ?
anim_frame	.byte ?
delay_frame	.byte ?
.send

.section bss
.align $100
cmd_buf		.fill $100
oam		.fill $100
.send

.strings instructions, 2, [format("  robotfindskitten v%s.%d", VERSION, nki_count), "     by Benjamin Gilbert", "       Original game by", "      Leonard Richardson", "   Released under the GPLv2", "", "", "In this game, you are robot.", "", "Your job is to find kitten.", "", "This task is complicated by", "the existence of various", "things which are not kitten.", "", "Robot must touch items to", "determine if they are kitten", "or not.", "", "The game ends when", "robotfindskitten.", "", "", "         PRESS START"]
.strings congratulations, 2, ["You found kitten!", "", "Way to go, robot!"]

FOUND_KITTEN_X = 16
FOUND_KITTEN_Y = 15
CONGRATS_X = 8
CONGRATS_Y = 20

.section fixed
palette
	.byte $0f, $10, $10, $10
	.byte $0f, $10, $10, $10
	.byte $0f, $10, $10, $10
	.byte $0f, $10, $10, $10
	.byte $0f, $11, $13, $16  ; last entry is found_kitten heart color
	.byte $0f, $14, $18, $1a
	.byte $0f, $1c, $21, $23
	.byte $0f, $25, $27, $2b

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
	ldy #0		; init cmd_buf offset
	.ccmd #CMD_OAM	; copy OAM buf to OAM
	sty cmd_off	; update offset

	; copy background and sprite palettes
	bit PPUSTATUS	; clear address latch
	.cp #>PALETTE_BG, PPUADDR ; address high
	.cp #<PALETTE_BG, PPUADDR ; address low
	ldx #0		; loop counter
-	lda palette,x	; get palette value
	sta PPUDATA	; write it
	inx		; increment
	cpx #$20	; are we done?
	bne -		; no; continue

	; render instructions
	instructions_ppu_off = 3 * 32 + 2
	ldy cmd_off	; get offset
	.ccmd #CMD_STRINGARR ; print string array
	lda #>instructions_ppu_off ; PPU offset high
	ora nametable	; add PPU base address
	.cmd		; write it
	.ccmd #<instructions_ppu_off ; PPU offset low
	.ccmd #instructions_bank ; string bank
	.ccmd #<instructions_addr ; string address low
	.ccmd #>instructions_addr ; string address high
	sty cmd_off	; update offset

	; enable render
	jsr run_nmi	; drawing instructions might take longer than VBLANK,
			; so don't enable render until the next NMI
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
	jmp found_kitten ; kitten; show ending animation

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
	jsr draw_entire_board ; redraw board
	ldx temp1	; restore X coord
	ldy temp2	; restore Y coord

	; update state
+	lda robot_x	; get old X coord
	sta cur_x	; store argument
	lda robot_y	; get new Y coord
	sta cur_y	; store argument
	stx robot_x	; write new X coord
	sty robot_y	; write new Y coord

	; clear old robot
	ldx #0		; load empty glyph
	jsr draw_robot	; clear robot

	; show new robot
	lda robot_x	; get X coord back
	sta cur_x	; save argument
	lda robot_y	; get Y coord back
	sta cur_y	; save argument
	ldx #ROBOT	; robot glyph
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

skip_mask .byte ((1 << BTN_A) | (1 << BTN_SELECT) | (1 << BTN_START))

; Show ending animation and load next board
found_kitten .proc
	; clear NKI text if showing
	jsr clear_nki

	; hide NKIs
	lda #$ff	; hide sprite
	ldx #0		; OAM offset
-	sta oam,x	; write Y coord
	inx		; increment for next OAM entry
	inx
	inx
	inx
	cpx #4 * NUM_NKIS ; check loop limit
	bne -		; continue until kitten

	; move kitten in Y
	lda #8 * FOUND_KITTEN_Y - 1 ; get Y coord
	sta oam + 4 * KITTEN_ITEM ; store it

	; set up loop
	.cp #3, anim_frame ; frames before collision

	; clear robot
next	lda robot_x	; get robot X
	sta cur_x	; store
	lda robot_y	; get robot Y
	sta cur_y	; store
	ldx #0		; empty glyph
	jsr draw_robot	; clear the robot

	; show robot
	lda #FOUND_KITTEN_X - 1 ; robot X in final frame
	sec		; set carry
	sbc anim_frame	; subtract for current frame
	sta robot_x	; store for clearing
	sta cur_x	; and for draw_robot
	lda #FOUND_KITTEN_Y ; get robot Y
	sta robot_y	; store for clearing
	sta cur_y	; and for draw_robot
	ldx #ROBOT	; robot glyph
	jsr draw_robot	; clear the robot

	; move kitten
	lda #FOUND_KITTEN_X ; kitten X in final frame
	clc		; clear carry
	adc anim_frame	; add for current frame
	asl		; multiply by 8
	asl
	asl
	sta oam + 4 * KITTEN_ITEM + 3 ; store to OAM X coord

	; update OAM
	ldy cmd_off	; get cmd_buf offset
	.ccmd #CMD_OAM	; update OAM
	sty cmd_off	; update offset

	; draw and wait
	jsr anim_wait	; NMI and wait one second
	bne done	; return on button press

	; next iteration
	dec anim_frame	; decrement frame counter
	bpl next	; continue until after robot and kitten meet

	; show heart in sprite 0
	.cp #8 * (FOUND_KITTEN_Y - 1) - 1, oam ; heart Y
	.cp #3, oam + 1	; heart glyph
	.cp #0, oam + 2 ; palette
	.cp #8 * (FOUND_KITTEN_X - 1), oam + 3 ; heart X
	ldy cmd_off	; get cmd_buf offset
	.ccmd #CMD_OAM	; update OAM

	; show congratulations
	congrats_ppu_off = CONGRATS_Y * 32 + CONGRATS_X
	.ccmd #CMD_STRINGARR ; print string array
	lda #>congrats_ppu_off ; PPU offset high
	ora nametable	; add PPU base address
	.cmd		; write it
	.ccmd #<congrats_ppu_off ; PPU offset low
	.ccmd #congratulations_bank ; string bank
	.ccmd #<congratulations_addr ; string address low
	.ccmd #>congratulations_addr ; string address high
	sty cmd_off	; update offset

	; NMI and wait for skip button
-	jsr run_nmi	; wait for frame
	jsr input	; query buttons
	lda new_buttons	; get result
	bit skip_mask	; check buttons
	beq -		; continue until pressed

	; clear congratulations
	; assume we are still in the correct bank
	lda #CONGRATS_Y	; starting Y coord to clear
	sta start_y	; store it
	clc		; clear carry
	adc congratulations_addr ; add line count
	sta end_y	; store end point
	jsr clear_lines	; clear lines

	; replace board
done	jmp next_board
	.pend

; Wait one second; bail out early if A, Select, or Start is pressed.
; Return: Z set if exited on timer, clear if exited on button.
anim_wait .proc
	lda #60		; frames per second
	sta delay_frame	; store
-	jsr run_nmi	; wait for frame
	jsr input	; check for button press
	lda new_buttons	; get buttons
	bit skip_mask	; check buttons
	bne return	; and return
	dec delay_frame	; decrement count
	bne -		; continue until done
return	rts
	.pend

; Show next board
next_board .proc
	jsr clear_nki	; clear any NKI that might be showing
	lda robot_x	; load X coord
	sta cur_x	; store argument
	lda robot_y	; load Y coord
	sta cur_y	; store argument
	ldx #0		; load empty glyph
	jsr draw_robot	; clear robot
	jsr make_board	; make a new board
	jsr place_robot	; place the robot
	.pend

; Clear any NKI that might be showing, preparatory to clearing board
clear_nki .proc
	lda nki_lines	; see if an NKI is showing
	bne +		; yes; continue
	rts		; no; return
+	clc		; clear carry
	adc nki_y	; compute end Y coord of NKI
	sta end_y	; and store it
	lda nki_y	; get start Y coord
	sta start_y	; and store it
	jsr clear_lines	; clear NKI
	.cp #0, nki_lines ; clear NKI indication
	jmp run_nmi	; wait for frame
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
